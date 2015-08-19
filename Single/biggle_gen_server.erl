%%%-------------------------------------------------------------------
%%% @author Noam <noam@noam-Inspiron-N5050>
%%% @copyright (C) 2015, Noam
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2015 by Noam <noam@noam-Inspiron-N5050>
%%%-------------------------------------------------------------------
-module(biggle_gen_server).

-behaviour(gen_server).

%% API
-export([start_link/0, create_world/2,get_close_chars/3, move_request/2, char_transition/2, fight/2, stop/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([run/5, stop/0]).

-define(SERVER, ?MODULE).
-define(min_char_distance, 0.2).

%%State variables:      state_name   - might not be neccesary, 
%			ets_table     - Id of biggle ets table, 
%			board_size   - maybe size of total board, 
%			board_limits - specifies specific biggle juristiction e.g {{x0,xn},{y0,yn}} 			
-record(state,     {state_name=default, ets_table, board_size, board_limits}). 
-record(character, {type, pid, location}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops server and all Character fsm's. 
%%
%% @spec stop() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop()-> stop(biggle_gen_server, 3).
stop(Own_pid, Reason) ->
	gen_server:call(Own_pid, {stop, Reason}).
%%--------------------------------------------------------------------
%% @doc
%% Create world
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
create_world(Pid,  {Board_size, Board_limits, Num_warrior, Num_zombies, Num_ww, Num_trees, Num_rocks} ) ->
	gen_server:call(Pid, {create_world, Board_size, Board_limits, Num_warrior, Num_zombies, Num_ww, Num_trees, Num_rocks}).


%%--------------------------------------------------------------------
%% @doc
%% Returns all characters close to Location
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
get_close_chars(Pid, Location, Radius)->
	gen_server:call(Pid, {get_close_chars, Location, Radius}).	

%%--------------------------------------------------------------------
%% @doc
%% Request permission to move to destination. 
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
move_request(Pid, {Curr_location, Destination, Type})->
	gen_server:call(Pid, {move_request, Curr_location, Destination, Type}).	

%%--------------------------------------------------------------------
%% @doc
%% After fight ends, each character updates whith results. 
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
char_transition(Pid, {Next_state})-> %Next_state can be dead or body or zombie(if animated).
	gen_server:call(Pid, {char_transition, Next_state}).	
%%--------------------------------------------------------------------
%% @doc
%% Atackee character updates biggle about fight so he can update multimedia.  
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------
fight(Pid, {Attackee_pid, Attacker_pid})-> 
	gen_server:call(Pid, {fight, Attackee_pid, Attacker_pid}).	

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %%Create Characters,
    TableId = ets:new(name,[bag]),	
    {ok, #state{ ets_table=TableId } }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({create_world, Board_size, Board_limits, Num_warrior, Num_zombies, Num_ww, Num_trees, Num_rocks}, _From, State) ->	
	%io:format("Creating world with: ~n Board size: ~p.~n Board limits: ~p.~n",[Board_size,Board_limits]),
	%io:format("On board:  Warriors: ~p.~n Zombies: ~p.~n White Walkers: ~p.~n Trees: ~p.~n Rocks: ~p.~n",
	%	  [Num_warrior, Num_zombies, Num_ww, Num_trees, Num_rocks]),		
	New_state=State#state{board_size=Board_size,board_limits=Board_limits },
	{characters_created, _, New_state1}=int_create_chars(New_state, [{trees, Num_trees},{rocks, Num_rocks}]), 
	{characters_created, _, _}=int_create_chars(New_state1, [{warrior,Num_warrior},{zombie,Num_zombies},
							     {white_walker,Num_ww}]),		
	Reply = [Char || {_,Char}<-ets:tab2list(New_state#state.ets_table)],
	{reply, Reply, New_state};

%Characters starting battle update biggle so he can update multimedia.  
handle_call({fight, Attackee_pid, Attacker_pid}, {_From_pid, _ref}, State)->
	multimedia_update(generate_fight_update({Attackee_pid, Attacker_pid}, State)),%%{fight, {{Type1, Name1}, {Type2, Name2}, Location}}
	{reply, ok, State};
 
handle_call({get_close_chars, Location, Radius}, {From_pid, _ref}, State) ->
	%%io:format("Handle call: get_close_chars: From= ~p.~n",[From_pid]),	
	Reply = int_close_chars(From_pid, Location, Radius, State#state.ets_table),
	{reply, Reply, State};


handle_call({char_transition, Next_state}, {From_pid, _ref}, State) -> %Next_state can by dead or body or zombie (if  animated). 
	char_transition(Next_state, From_pid, State), 
	{reply, ok, State};

handle_call({stop, Reason},_From, State) ->
	%io:format("biggle_master handle call stop, reason: ~p.~n",[Reason]), 
	{stop, normal, ok, State}; 

handle_call({move_request, Curr_location, Destination, Type}, {From_pid, _ref}, State) ->
	%%Check if destination free: 
	Close_chars= int_close_chars(From_pid, Destination, ?min_char_distance, State#state.ets_table),	
	case Close_chars of
		[] ->%space free, allow movment. 
			Reply={ok, Destination},
			update_char_location(State#state.ets_table, From_pid, Destination, Type),  
			multimedia_update({movement, {Type, From_pid, Curr_location, Destination}});
		_Not_free ->
			Reply={denied, {}}
	end, 
	{reply, Reply, State}.


%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
	%io:format(" voodoo voodoo handle_cast: ~p.~n",[Msg]),	
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
	%io:format(" voodoo voodoo Unexpected message: ~p.~n",[Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
	%io:format("In terminate: Biggle master received Stop.~n Reason: ~p~n ",[Reason]),
	%Stop character Fsm's,	
	int_kill_chars(Reason, State#state.ets_table), 
	%Delete Ets.     
	ets:delete(State#state.ets_table),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%Generate fight update to multimedia according to format: {fight, {{Type1, Name1}, {Type2, Name2}, Location}}
generate_fight_update({Attackee_pid, Attacker_pid}, State)->
	[{_,{_, Type1, _, {X1,Y1}}}]=ets:lookup(State#state.ets_table, Attackee_pid),
	[{_,{_, Type2, _, {X2,Y2}}}]=ets:lookup(State#state.ets_table, Attacker_pid),
	Location={ trunc((X1+X2)/2), trunc((Y1+Y2)/2)}, 
	{fight, {{Type1, Attackee_pid}, {Type2, Attacker_pid}, Location}}. 

print_ets(Ets_table)->
	L=[{Type,Location2, Pid} || {_,{_,Type,Pid,Location2}}<-ets:tab2list(Ets_table)].
	%io:format("Printing all ets:~n"),
	%io:format("~p~n",[L]).	

%update multimedia server if exists.
multimedia_update(Msg)->
	%update multimedia server if exists.
	case (whereis(server_gate)) of  
		'undefined' ->
			do_nothing; 
		_Else       ->
			server_gate ! Msg%%Update multimedia
	end.		

%Update Ets table according to transition of character to dead or body or zombie. 
char_transition(Dead_body_zom ,From_pid, State)->
	%multimedia_update(DeadORbody, {From_pid}), 
	update_char_type(State#state.ets_table, Dead_body_zom, From_pid).
	
	
%Change character type as outcome of battle or body death/ressurection, update sever_gate (multimedia and statistics).	 
update_char_type(Ets_table, New_type, From_pid)->
	case New_type of
		dead ->	
			print_ets(Ets_table),
			[{_,{_, Type, _, _}}]=ets:lookup(Ets_table, From_pid),			
			Del=ets:delete(Ets_table, From_pid),
			print_ets(Ets_table), 			
			%io:format("Deleted dead ~p, Del=~p",[From_pid, Del]),
			multimedia_update({death,{Type, From_pid}}); 
		_Else->%change to body or zombie
			%{_Character, _Type, From_pid, Location}={character, k, From_pid, {0,0}},
			print_ets(Ets_table),			
			[{_,{_, Original_type, From_pid, Location}}]=ets:lookup(Ets_table, From_pid),
			%%io:format("In update_char_type Match=~p, pid=~p~n",[Match, From_pid]),			
			Del=ets:delete(Ets_table, From_pid),
			%io:format("In update_char_type Del=~p, pid=~p~n",[Del, From_pid]),					
			print_ets(Ets_table),			
			Char_rec=#character{type=New_type, pid=From_pid, location=Location},			
			ets:insert(Ets_table, {From_pid, Char_rec}), 
			print_ets(Ets_table),
			multimedia_update({transition, {Original_type, New_type, From_pid, Location}})
	end.	
	

%Change character location in Ets as result of movment. 
update_char_location(Ets_table, Pid, New_location, Type) ->  	
	Del=ets:delete(Ets_table, Pid),
	%io:format("update_char_loc ~p to ~p in biggle: Del= ~p.~n",[Pid, Type, Del]),	
	Char_rec=#character{type=Type, location=New_location, pid=Pid}, 	
	%%io:format("In update_char_location: new char rec: ~p~n",[Char_rec]),	
	ets:insert(Ets_table, {Pid, Char_rec}). 
  
%Stop character Fsm's,	
int_kill_chars(Reason, Ets) ->
	[ char_gfsm:stop(Pid, Reason) || {_,{_,_, Pid ,_}}<-ets:tab2list(Ets), Pid/='undefined']. 	 

%%Return close characters. 
int_close_chars(Location1, Radius, Ets)->
	[ {Type,Location2, Pid} || {_,{_,Type,Pid,Location2}}<-ets:tab2list(Ets), int_dist(Location1,Location2)<Radius]. 	

int_close_chars(Requestor_pid, Location1, Radius, Ets)->
	[ {Type,Location2, Pid} || {_,{_,Type,Pid,Location2}}<-ets:tab2list(Ets), Requestor_pid/=Pid, int_dist(Location1,Location2)<Radius]. 	

%%Calculate distance between two locations. 
int_dist({X1,Y1}, {X2,Y2})->
	math:sqrt( math:pow(X1-X2,2) + math:pow(Y1-Y2,2) ). 	

%%Get a random free space whithin biggle masters area whith a radius of 1 clear around it.
int_get_rand_free_space(State)->
	{{X0,Xn},{Y0,Yn}}=State#state.board_limits, %area of juristiction for specific biggle.
	%io:format("In: get_rand_free_space(state) State#state.board_limits= ~p~n",[State#state.board_limits]),		
	{X,New_seed}=random:uniform_s( Xn ,erlang:now()),
	{Y,_}=random:uniform_s( Yn-Y0 ,New_seed),
	case ( int_close_chars({X0+X,Y0+Y}, 1, State#state.ets_table) ) of
		[] -> {X0+X,Y0+Y}; 			%if free return location.
		_  -> int_get_rand_free_space(State)    %else try again.
	end.

%Create characters randomly on board according to list with elements: {type, Num}.
int_create_chars(State, [])->
	{characters_created, 'no info', State}; %'no info'- Future info.

int_create_chars(State, [{_Type, 0}|List_of_chars])->
	int_create_chars(State, List_of_chars);

int_create_chars(State, [{Type, Num}|List_of_chars])->
	Dest=int_get_rand_free_space(State),
	int_set_char(State#state.ets_table, int_make_char_record(State#state.board_size, Type, Dest)),
	case (Num-1) of %Call again
		0  -> int_create_chars(State, List_of_chars);
		_  -> int_create_chars(State, [{Type, Num-1}|List_of_chars]) 
	end. 

%Adds a character record to ets table. 
int_set_char(Ets, New_char_record)->
	ets:insert(Ets, {New_char_record#character.pid, New_char_record}).

%Create new character recored, spawn fsm and update multimedia. 
int_make_char_record(Board_size, Type, Location) ->
	case Type of 
		rocks ->
			Char=#character{type=Type, location=Location}, 
			Pid=spawn(erlang, is_integer, [1]);		
		trees ->
			Char=#character{type=Type, location=Location}, 
			Pid=spawn(erlang, is_integer, [1]);		
		_Character ->
			{ok, Pid}=char_gfsm:start_link({biggle_gen_server, Board_size, Type, Location}),
			Char=#character{type=Type, location=Location, pid=Pid}
	end,  	
	multimedia_update({born, {Type, Pid, Location}}),
	Char.  



run(W,WW,Z, T, R)-> 
	start_link(),
	create_world(biggle_gen_server, {1000, {{0,200},{0,200}}, W,WW,Z,T,R}).
