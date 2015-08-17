%%%-------------------------------------------------------------------
%%% @author Noam <noam@noam-Inspiron-N5050>
%%% @copyright (C) 2015, Noam
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2015 by Noam <noam@noam-Inspiron-N5050>
%%%-------------------------------------------------------------------
-module(biggle_master).

-behaviour(gen_server).

-export([divide_params_per_biggle/7]).
	
%% API
-export([start_link/1, start_biggles/2, remote_close_char/2, stop/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(biggle_1, 'biggle_1@192.168.0.102').
-define(biggle_2, 'biggle_2@192.168.0.102').
-define(biggle_3, 'biggle_3@192.168.0.102').
-define(biggle_4, 'biggle_4@192.168.0.102').

-record(state, {num_biggles, biggle_list, board_size}).

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
start_link(Num_of_biggles) ->
	%io:format("Enter simulation parameters: ~n"),
	%io:format("{Board_size, Num_warriors, Num_zombies, Num_ww, Num_trees, Num_rocks}"),  	
	gen_server:start_link({global, ?SERVER}, ?MODULE, [Num_of_biggles], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_biggles(Pid, {Board_size, Num_warrior, Num_zombies, Num_ww, Num_trees, Num_rocks}) ->
    gen_server:call(Pid, {start_biggles, Board_size, Num_warrior, Num_zombies, Num_ww, Num_trees, Num_rocks}).

%%--------------------------------------------------------------------
%% @doct
%% Stops server beagle servers. 
%%
%% @spec stop() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop(Own_pid, Reason) ->
	gen_server:call(Own_pid, {stop, Reason}).
%%--------------------------------------------------------------------
%% @doct
%% Returns close characters from other biggles. 
%%
%% @spec remote_close_char -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
remote_close_char(Own_pid, {Location, Radius, From_pid, Ref}) ->
	gen_server:cast(Own_pid, {remote_close_char, Location, Radius, From_pid, Ref}).
	
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
init([Num_of_biggles]) ->
	%Biggle_list=[list_to_atom( string:concat("biggle_", X) ) ||X<-lists:seq(1,Num_of_biggles)], 
	Biggle_list=[{global, list_to_atom( "biggle_"++ integer_to_list(X) ) } ||X<-lists:seq(1,Num_of_biggles)], 
	
	%[ biggle_gen_server:start_link( lists:nth(X, Biggle_list) ) || X<-lists:seq(1,Num_of_biggles)],  
	{_,PID1} = rpc:call(?biggle_1, biggle_gen_server, start_link, [biggle_1]), 
	io:format("Pid is ~p, is it PID?:~p",[PID1,is_pid(PID1)]),
	%register(biggle_1, PID1),
	{_,PID2} = rpc:call(?biggle_2, biggle_gen_server, start_link, [biggle_2]),
	%register(biggle_2, PID2),
	{_,PID3} = rpc:call(?biggle_3, biggle_gen_server, start_link, [biggle_3]),
	%register(biggle_3, PID3),
	{_,PID4} = rpc:call(?biggle_4, biggle_gen_server, start_link, [biggle_4]),	
	%register(biggle_4, PID4),
	{ok, #state{num_biggles=Num_of_biggles, biggle_list=Biggle_list}}.

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
%Recieve parameters for all biggles and start each biggle with his own part of the board and data. 
handle_call({start_biggles, Board_size, Num_warrior, Num_zombies, Num_ww, Num_trees, Num_rocks}, _From, State) ->
	List_per_biggle=divide_params_per_biggle(State#state.num_biggles, Board_size, Num_warrior, Num_zombies, Num_ww, Num_trees, 		Num_rocks),    
	
%	biggle_gen_server:create_world( biggle_1, lists:nth(1, List_per_biggle) ), 	
	[ biggle_gen_server:create_world(  lists:nth(X, State#state.biggle_list), lists:nth(X, List_per_biggle) ) 
	  || X<-lists:seq(1,State#state.num_biggles)], 	
	{reply, ok , State};

%Stops all processes down command chain. 
handle_call({stop, Reason},_From, State) ->
	io:format("biggle master handle call stop, reason: ~p.~n",[Reason]), 
	{stop, normal, ok, State}. 



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({remote_close_char, Location, Radius, From_pid, Ref}, State)->
	Remote=return_remote_chars(Location, Radius, From_pid, State), 	
	biggle_gen_server:close_chars_ans(From_pid, {Ref, Remote}), 	
	%io:format("master: cast remote_close not returning yet, but remotes are ~p~n",[Remote]), 	
	{noreply, State};

handle_cast(Msg, State) ->
	io:format("biggle_master handle cast ?? ~p ~p", [Msg, State]),    
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
handle_info(_Info, State) ->
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
	io:format("In terminate: Biggle master received Stop.~n Reason: ~p~n ",[Reason]),
	%stop biggle servers:
	io:format("in master terminate, ~p~n",[State#state.biggle_list]),
	[biggle_gen_server:stop(lists:nth(X, State#state.biggle_list), normal) || X<-lists:seq(1,State#state.num_biggles)], 
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

%Return remote close chars to biggle From_pid, from other biggles. 
return_remote_chars(Location, Radius, From_pid, State)->
	Registered_name=proplists:get_value(registered_name,erlang:process_info(From_pid)),
	Remote_reply_lists=[biggle_gen_server:get_close_chars_master(lists:nth(X,State#state.biggle_list), Location, Radius)||	
	X<-lists:seq(1,State#state.num_biggles), lists:nth(X, State#state.biggle_list)/=Registered_name  ],
	Remote_reply=lists:flatten(Remote_reply_lists),   	
	%io:format("Currently Returning remote chars, Remote_reply: ~p, requestor is ~p.~n",[Remote_reply, Registered_name]),
	Remote_reply. 
	
%Return list with all biggle parameters, currently assuming 4 biggles exactly. 
divide_params_per_biggle(Num_of_biggles, Board_size, Num_warrior, Num_zombies, Num_ww, Num_trees, Num_rocks)->
	case (Num_of_biggles) of
		4->
			Low_lim=round(Board_size/2),
			Up_lim=Board_size, 
			Warriors=round(Num_warrior/Num_of_biggles), 
			Zombies= round(Num_zombies/Num_of_biggles),
			Ww=      round(Num_ww/Num_of_biggles), 
			Trees=   round(Num_trees/Num_of_biggles),
			Rocks=   round(Num_rocks/Num_of_biggles), 		
			Warriors_left=Num_warrior-(Num_of_biggles-1)*Warriors, 
			Zombies_left=Num_zombies-(Num_of_biggles-1)*Zombies, 
			Ww_left=Num_ww-(Num_of_biggles-1)*Ww, 
			Trees_left=Num_trees-(Num_of_biggles-1)*Trees, 
			Rocks_left=Num_rocks-(Num_of_biggles-1)*Rocks, 	
			[{Board_size, {{0, Low_lim},      {0,  Low_lim}},     Warriors, Zombies, Ww, Trees, Rocks },  
			 {Board_size, {{Low_lim, Up_lim}, {0,  Low_lim}},     Warriors, Zombies, Ww, Trees, Rocks },			
			 {Board_size, {{0, Low_lim},      {Low_lim, Up_lim}}, Warriors, Zombies, Ww, Trees, Rocks },
			 {Board_size, {{Low_lim, Up_lim}, {Low_lim, Up_lim}}, Warriors_left, Zombies_left, Ww_left, Trees_left, Rocks_left }];

		1->
			[{Board_size, {{0, Board_size},{0, Board_size}}, Num_warrior, Num_zombies, Num_ww, Num_trees, Num_rocks}];
		_Else->
			io:format("Error: Currently supporting 4 biggles!")
	end.














