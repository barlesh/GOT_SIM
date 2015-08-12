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
-export([start_link/1, stop/2, attack/2]).
	%%, , attacked/2 
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	 terminate/3, code_change/4,
% custom state names
	 idle/2,idle/3, waking/2, waking/3, attacking/2, 
	 attacked/2]).
	%attacked/3, attacking/3,

-define(SERVER, ?MODULE       ).
-define(idle_period, 5000     ).
-define(attack_timeout, 4000  ). 
-define(min_char_distance, 0.2). 
-define(idle_factor, 1000     ).
-record(state, {name, biggle, type, location, pid, radius=15, step_size=30, attackee}).

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
%% stops charactar and terminates. 
%% 
%% @spec stop(Own_pid, Biggle_master) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop(Own_pid, Reason) ->
	gen_fsm:stop(Own_pid, Reason).    

%%--------------------------------------------------------------------
%% @doc
%% Allowes other fsm's to attack. Ask for an attack session,  Returns when/if the other accepts
%% 
%% @spec stop(Own_pid, Biggle_master) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

%% ask for a begin session. Returns when/if the other accepts
attack(OwnPid, OtherPid) ->
	gen_fsm:sync_send_event(OwnPid, {attacked, OtherPid}, ?attack_timeout).

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
	{Timeout,_}=random:uniform_s(?idle_factor, os:timestamp()), 
	{ok, idle, #state{biggle=Biggle_pid, type=Type, location=Location, pid=Own_pid}, ?idle_period+Timeout}.

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
	{next_state, waking, State, 0};

idle({attacked, Other_pid}, State)->
	io:format("Idle/2: Pid ~p, Type: ~p, attacked by ~p.~n",[State#state.pid, State#state.type, Other_pid]),	
	{next_state, attacked, State, 0};

idle(Event, State) ->
	unexpected(Event, idle),
	{next_state, idle, State, ?idle_period}.


waking(timeout, State) ->
	Close_chars=biggle_gen_server:get_close_chars(State#state.biggle, State#state.location, State#state.radius),	
	Close_list=tuple_to_list(list_to_tuple(Close_chars)),
	io:format("Type: ~p, Pid: ~p, Location: ~p, close chars: ~p~n", [State#state.type,State#state.pid,State#state.location, Close_list]),
	%io:format("Type: ~p, Pid: ~p, Action: ~p~n", [State#state.type,State#state.pid, Action]),	
	Action=choose_action(State#state.type, Close_list, State#state.location, State#state.step_size),
	{Next_state, New_state_rec, Timeout}=preform_action(State, Action), 
	{next_state, Next_state, New_state_rec, Timeout};

waking({attacked, Other_pid}, State)-> %Not supposed to be possible
	io:format("Waking/2: Pid ~p, Type: ~p, attacked by ~p.~n",[State#state.pid, State#state.type, Other_pid]),	
	{next_state, attacked, State, 0};
	
waking(Event, Data) ->
	unexpected(Event, waking),
	{next_state, idle, Data, ?idle_period}.

attacking(timeout, State) ->%Attacked and did not get reply before timeout. 		
	io:format("Type: ~p, Pid: ~p, attacked ~p but got no reply.",[State#state.type,State#state.pid,State#state.attackee]),
	{next_state, idle, State#state{attackee='undefined'}, ?idle_period};

attacking({attacked, Other_pid}, State)->%Attacked when attacking. 
	
	io:format("attacking/2: Pid ~p, Type: ~p, attacked by ~p.~n, disregarding.",[State#state.pid, State#state.type, Other_pid]),	
	{next_state, attacked, State, 0};

attacking(Event, Data)->	
	unexpected(Event, attacking),
	{next_state, idle, Data, ?idle_period}.

attacked(timeout, State) ->		
	{next_state, waking, State, 0};

attacked({attacked, Other_pid}, State)->
	io:format("attacked/2: Pid ~p, Type: ~p, attacked by ~p.~n",[State#state.pid, State#state.type, Other_pid]),	
	{next_state, attacked, State, 0};

attacked(Event, Data)->	
	unexpected(Event, attacked),
	{next_state, idle, Data, ?idle_period}.

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

idle({attacked, Other_pid}, {From_pid, _Ref}, State) -> 
	io:format("In Idle: Pid ~p, Type: ~p, attacked by ~p.~n",[State#state.pid, State#state.type, From_pid]),	
	Reply = "Currently cannont be attacked.",
	{Timeout,_}=random:uniform_s(2, os:timestamp()), 
	{reply, Reply, idle, State, ?idle_period+Timeout};

idle(Event, _From, State) ->
	unexpected(Event, State), 	
	Reply = ok,
	{reply, Reply, idle, State}.


waking({attacked, Other_pid}, {From_pid, _Ref}, State) -> %{From_pid, _Ref}
	io:format("In waking: Pid ~p, Type: ~p, attacked by ~p.~n",[State#state.pid, State#state.type, From_pid]),	
	Reply = "Currently cannont be attacked.",
	{Timeout,_}=random:uniform_s(2, os:timestamp()), 
	{reply, Reply, idle, State, ?idle_period+Timeout};

waking(Event, _From, State) ->
	io:format("@@@@@Received unreal event!"),
	unexpected(Event, State),
	Reply=ok, 	
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
handle_event(termination, StateName, State) ->
    {next_state, StateName, State};

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
%handle_sync_event(get_vicinity, Own_pid, waking, State)->
%	biggle_gen_fsm:get_close_characters(State#state.biggle, State#state.location, State#state.radius);

handle_sync_event(_Event, _From, StateName, State) ->
	io:foramt("handle_sync_event called.~n"),
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
terminate(Reason, State) ->
	io:format("~p terminatig, ~p~n",[State#state.pid, Reason]),    
	ok.

terminate(Reason, _StateName, State) ->
	io:format("~p terminatig, ~p~n",[State#state.pid, Reason]),    
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
	io:format("~p @@@@@@@@@@@@@@@@@@@@@@@@@@received unknown event ~p while in state ~p~n",[self(), Msg, State]).

%%Calculate distance between two locations. 
dist({X1,Y1}, {X2,Y2})->
	math:sqrt( math:pow(X1-X2,2) + math:pow(Y1-Y2,2) ). 	

%%Return close characters. 
close_chars(Close_chars, Location1, Radius)->
	[ {Type,Location2, Pid} || {Type,Location2,Pid}<-Close_chars, dist(Location1,Location2)<Radius]. 	



preform_action(State, {move, {Destination}}) ->
	{Reply, New_location}=biggle_gen_server:move_request(State#state.biggle, {State#state.location, Destination, State#state.type}), 		
	case (Reply) of
		'ok' ->
			New_state_rec=State#state{location=Destination};
		'denied' ->
			New_state_rec=State
	end, 	 
	{Timeout,_}=random:uniform_s(200, os:timestamp()),
	{idle, New_state_rec, ?idle_period+Timeout};

%Performing an attack is done by changing to attacking mode. 
preform_action(State, {attack, {{Etype, Location1, Other_pid}} }) ->
	%Reply=gen_fsm:sync_send_event(Other_pid, {attacked, Own_pid}, ?attack_timeout),	
	gen_fsm:send_event(Other_pid, {attacked, State#state.pid}),
	{attacking, State#state{attackee={Etype, Location1, Other_pid}}, ?attack_timeout}.

%Character chooses where to walk to. 
get_destination(Close_chars, {X0,Y0}, Step_size)->
	{RandX, Seed1}=random:uniform_s(os:timestamp()), 		
	{RandY, _}=random:uniform_s(Seed1),
	{X1,Y1}={X0 + RandX*Step_size, Y0 + RandY*Step_size}, %random close destination. 
	Too_close=close_chars(Close_chars, {X1,Y1}, ?min_char_distance), %check if any char closer than min dist. 	
	case Too_close of
		[] ->   %Destination free, return it. 
			{X1,Y1}; 
		_Else -> %Destination occupied, try agian. 
			get_destination(Close_chars, {X0,Y0}, Step_size) 
	end. 

choose_action(body, _Close_chars, _Location, _Step_size) ->
		do_nothing; 
choose_action(white_walker, Close_chars, Location, Step_size) ->
	Bodies = [], %[ {Etype, Location1, Pid} || {Etype, Location1, Pid}<-Close_chars, Etype=:=body], 
	Enemys = [], %[ {Etype, Location1, Pid} || {Etype, Location1, Pid}<-Close_chars, Etype=:=warrior],	
	case {Bodies,Enemys} of 
		{[],[]}     -> %Move
			Destination=get_destination(Close_chars, Location, Step_size),
			{move, {Destination}}; %If no one to attack, move 
		{[],Enemys} -> %Attack	
			Atackee = random:uniform(length(Enemys)),
			{attack, {lists:nth(Atackee, Enemys)}};
		{Bodies, _} -> %Reanimate
			Animatee = random:uniform(length(Bodies)), 
			{animate, {lists:nth(Animatee, Bodies)}}
	end; 
choose_action(Type, Close_chars, Location, Step_size) ->
	%Attack: 
	case Type of 
		warrior ->
			Enemys  = [];			
			%Enemys = [ {Etype, Location1, Pid} || {Etype, Location1, Pid}<-Close_chars, Etype=:=zombie orelse Etype=:=white_walker];
		zombie  ->
			Enemys = []
			%Enemys = [ {Etype, Location1, Pid} || {Etype, Location1, Pid}<-Close_chars, Etype=:=warrior]	
	end, 
	case (Enemys) of	 
		[] -> 
			Destination=get_destination(Close_chars, Location, Step_size),
			{move, {Destination}}; %If no one to attack, move 
		Enemys -> 	
			Atackee = random:uniform(length(Enemys)),
			{attack, {lists:nth(Atackee, Enemys)}}
	end.  
	











