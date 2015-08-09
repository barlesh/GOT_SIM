%%%-------------------------------------------------------------------
%%% @author Noam <noam@noam-Inspiron-N5050>
%%% @copyright (C) 2015, Noam
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2015 by Noam <noam@noam-Inspiron-N5050>
%%%-------------------------------------------------------------------
-module(char_gfsm).

-behaviour(gen_fsm).

%% gen_fsm callbacks
%% Character states: Sleeping, Waking, Attacking, Attacked.

%% Public API:
-export([start_link/1, get_vicinity/2]).
	%%, attack/2, attacked/2 
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	 terminate/3, code_change/4,
% custom state names
	 idle/2,idle/3, waking/2, waking/3]). 
	 %, attacking/2, 
	 %attacking/3, attacked/2, attacked/3]).


-define(SERVER, ?MODULE).

-record(state, {name, biggle, type, location, pid, radius=10}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link({Biggle_pid, Type, Location}) ->
    gen_fsm:start_link(?MODULE, [Biggle_pid, Type, Location], []).

%%--------------------------------------------------------------------
%% @doc
%% When waking up, start new round by asking for characters in vicinity.
%% 
%% @spec get_vicinity(Own_pid, Biggle_master) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_vicinity(Own_pid, Biggle_master) ->
    gen_fsm:sync_send_event(Own_pid, {get_vicinity, Biggle_master}, 10000).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Biggle_pid, Type, Location]) ->
	Own_pid=self(),	
	{ok, idle, #state{biggle=Biggle_pid, type=Type, location=Location, pid=Own_pid}, 3000}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------

idle(timeout, State) ->
	%io:format("~p of type ~p received TO in idle.~n", [State#state.pid, State#state.type]),		
	{next_state, waking, State, 0};

idle(Event, Data) ->
	unexpected(Event, idle),
	{next_state, idle, Data}.

waking(timeout, S) ->
	%io:format("~p of type ~p received TO in waking.~n", [S#state.pid,S#state.type]),
	Close_chars=biggle_gen_server:get_close_chars(S#state.biggle, S#state.location, 1),%S#state.radius
	io:format("Close_chars of ~p location: ~p are: ~p~n", [S#state.pid,S#state.location, Close_chars]),
	%gen_fsm:sync_send_event(S#state.pid, {get_vicinity, S#state.biggle}, 1000),
	{next_state, idle, S, 9000}.
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
idle(timeout, _From, State) ->
	Reply = ok,
	{reply, Reply, idle, State}.

waking(_Event, _From, State) ->
	Reply = ok,
	{reply, Reply, idle, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(get_vicinity, Own_pid, waking, State)->
	biggle_gen_fsm:get_close_characters(State#state.biggle, State#state.location, State#state.radius);

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Unexpected allows to log unexpected messages
unexpected(Msg, State) ->
io:format("~p received unknown event ~p while in state ~p~n",
[self(), Msg, State]).
