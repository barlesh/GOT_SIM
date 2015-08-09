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
-export([start_link/0, create_world/2,get_close_chars/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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
	io:format("Creating world with: ~n Board size: ~p.~n Board limits: ~p.~n",[Board_size,Board_limits]),
	io:format("On board:  Warriors: ~p.~n Zombies: ~p.~n White Walkers: ~p.~n Trees: ~p.~n Rocks: ~p.~n",
		  [Num_warrior, Num_zombies, Num_ww, Num_trees, Num_rocks]),		
	New_state=State#state{board_size=Board_size,board_limits=Board_limits },
	{characters_created, Info, New_state1}=int_create_chars(New_state, [{trees, Num_trees},{rocks, Num_rocks}]), 
	{characters_created, Info, New_state2}=int_create_chars(New_state1, [{warrior,Num_warrior},{zombies,Num_zombies},
							     {white_walkers,Num_ww}]),		
	Reply = ['not yet spawned, only placed on board.', ets:tab2list(New_state#state.ets_table)],
	{reply, Reply, New_state};


handle_call({get_close_chars, Location, Radius}, _From, State) ->
	Reply = int_close_chars(Location, Radius, State#state.ets_table),
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
handle_cast(_Msg, State) ->
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
	io:format("Unexpected message: ~p.~n",[Info]),
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
terminate(_Reason, _State) ->
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

%%Return close characters. 
int_close_chars(Location1, Radius, Ets)->
	[ {Type,Location2} || {Char,Type,_,Location2}<-ets:tab2list(Ets), int_dist(Location1,Location2)<Radius]. 	

%%Calculate distance between two locations. 
int_dist({X1,Y1}, {X2,Y2})->
	math:sqrt( math:pow(X1-X2,2) + math:pow(Y1-Y2,2) ). 	

%%Get a random free space whithin biggle masters area whith a radius of 1 clear around it.
int_get_rand_free_space(State)->
	{{X0,Xn},{Y0,Yn}}=State#state.board_limits, %area of juristiction for specific biggle.
	io:format("In: get_rand_free_space(state) State#state.board_limits= ~p~n",[State#state.board_limits]),		
	{X,New_seed}=random:uniform_s( Xn ,erlang:now()),
	{Y,_}=random:uniform_s( Yn-Y0 ,New_seed),
	case ( int_close_chars({X0+X,Y0+Y}, 1, State#state.ets_table) ) of
		[] -> {X0+X,Y0+Y}; 			%if free return location.
		_  -> int_get_rand_free_space(State)    %else try again.
	end.

%Create characters randomly on board according to list with elements: {type, Num}.
int_create_chars(State, [])->
	{characters_created, 'no info', State}; %'no info'- Future info.

int_create_chars(State, [{Type, Num}|List_of_chars])->
	Dest=int_get_rand_free_space(State),
	int_set_char(State#state.ets_table, int_make_char_record(Type, Dest)),
	case (Num-1) of %Call again
		0 -> int_create_chars(State, List_of_chars);
		_ -> int_create_chars(State, [{Type, Num-1}|List_of_chars]) 
	end. 

%Adds a character record to ets table. 
int_set_char(Ets, New_char_record)->
	ets:insert(Ets, New_char_record).

%Create new character recored. 
int_make_char_record(Type, Location) ->
	%%%INSERT NAME WHEN STARTING LINK
	{ok, Pid}=char_gfsm:start_link({biggle_gen_server, Type, Location}),
	#character{type=Type, location=Location, pid=Pid}. %add pid





