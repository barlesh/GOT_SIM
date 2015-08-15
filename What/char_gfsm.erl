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
	 attacked/2, body/2, dead/2, animating/2]).
	%attacked/3, attacking/3,

-define(SERVER,        ?MODULE).
-define(idle_period,      1000).  
-define(attack_timeout,   4000). 
-define(min_char_distance,   2). 
-define(idle_factor,      1000).
-define(animation_timeout,  300).
-define(body_timeout,    8000).

-record(state, {biggle, board_size, type, location, pid, radius=50, step_size=80, attackee, attacker, bodies}).
										 %attacker=pid, bodies for ww. 
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
start_link({Biggle_pid, Board_size, Type, Location}) ->
    gen_fsm:start_link(?MODULE, [Biggle_pid,Board_size, Type, Location], []).

%%--------------------------------------------------------------------
%% @doc
%% stops charactar and terminates. 
%% 
%% @spec stop(Own_pid, Biggle_master) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop(Own_pid, _Reason) ->
	gen_fsm:send_all_state_event(Own_pid, stop). 	


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
init([Biggle_pid, Board_size, Type, Location]) ->
	Own_pid=self(),	
	{ok, idle, #state{biggle=Biggle_pid, board_size=Board_size, type=Type, location=Location, pid=Own_pid}, timeout()}.
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

idle({battle_outcome, Result, Other_pid}, State)->
	%%io:format("Idle/2: Pid ~p, Type: ~p, received battle outcome ~p, from ~p~n, %%Need suprresion%%",	
	%[State#state.pid, State#state.type, Result, Other_pid]),		
	{next_state, idle, State, timeout(idle)};

idle({attacked, Other_pid}, State)->
	%%io:format("Idle/2: Pid ~p, Type: ~p, attacked by ~p.~n",[State#state.pid, State#state.type, Other_pid]),		
	{next_state, attacked, State#state{attacker=Other_pid}, 0};

idle({animation_request, Other_pid}, State)->
	%%io:format("Idle/2:~p ~p, got animation_request from ~p~n, %%suprresion%%",[State#state.type, State#state.pid, Other_pid]),
	{next_state, idle, State, timeout(idle)};
	
idle(Event, State) ->
	unexpected(Event, idle),
	{next_state, idle, State, timeout(idle)}.
	

waking(timeout, State) ->
	Close_chars=biggle_gen_server:get_close_chars(State#state.biggle, State#state.location, State#state.radius),	
	Close_list=tuple_to_list(list_to_tuple(Close_chars)),
	%%io:format("Type: ~p, Pid: ~p, Location: ~p, close chars: ~p~n", [State#state.type,State#state.pid,State#state.location, Close_list]),
	%%io:format("Type: ~p, Pid: ~p, Action: ~p~n", [State#state.type,State#state.pid, Action]),	
	Action=choose_action(State#state.type, State#state.board_size, Close_list, State#state.location, State#state.step_size),
	{Next_state, New_state_rec, Timeout}=preform_action(State, Action), 
	{next_state, Next_state, New_state_rec, Timeout};

waking({attacked, Other_pid}, State)-> %Not supposed to be possible
	%%io:format("Waking/2: Pid ~p, Type: ~p, attacked by ~p.~n",[State#state.pid, State#state.type, Other_pid]),	
	{next_state, attacked, State, 0};

waking({battle_outcome, Result, Other_pid}, State)->
	%%io:format("waking/2: Pid ~p, Type: ~p, received battle outcome ~p, from ~p~n, %%Need suprresion%%",	
	%[State#state.pid, State#state.type, Result, Other_pid]),		
	{next_state, idle, State, timeout(idle)};

waking({animation_request, Other_pid}, State)->
	%%io:format("Waking/2:~p ~p, got animation_request from ~p~n, %%suprresion%%",[State#state.type, State#state.pid, Other_pid]),
	{next_state, idle, State, timeout(idle)};
	
waking(Event, Data) ->
	unexpected(Event, waking),
	{next_state, idle, Data, timeout(idle)}.

attacking(timeout, State) ->%Attacked and did not get reply before timeout. 		
	%%io:format("Type: ~p, Pid: ~p, attacked ~p but got no reply.~n",[State#state.type,State#state.pid,State#state.attackee]),
	{next_state, idle, State#state{attackee='undefined'}, timeout(idle)};

attacking({attacked, Other_pid}, State)->%Attacked when attacking. 
	{_,_,Attackee_pid}=State#state.attackee, 	
	case (Other_pid=:=Attackee_pid) of
		false->%Decline attack
			%%io:format("attacking/2: Pid ~p, attacked by ~p, disregarding.~n",[State#state.pid, Other_pid]),	
			gen_fsm:send_event(Other_pid, {attack_refused, State#state.pid}), 
			{Next_state, New_state, Timeout}={attacking, State, timeout(attack)};
		true-> %Attacked by attackee - TODO:
			%%io:format("Attacking/2, ~p attacked by attackee ~p.~n",[State#state.pid, Other_pid]),			
			case (State#state.type) of	
				warrior->%Warrior refusers attack. 
					%%io:format("attacking/2: Pid ~p ,attacked by atackee ~p.~n, refusing.",[State#state.pid, Other_pid]),
					gen_fsm:send_event(Other_pid, {attack_refused, State#state.pid}),
					{Next_state, New_state, Timeout}={attacked, State#state{attackee=undefined, attacker=Other_pid}, 0};
				_Else  ->%White walker or zombie continue attack
					{Next_state, New_state, Timeout}={attacked, State, 0}
			end	
	end, 				
	{next_state, Next_state, New_state, Timeout};

attacking({attack_refused, Other_pid}, State)->%Attackee refuses attack:
	%%io:format("attacking/2: Pid ~p, refused attack by ~p.~n. ",[Other_pid, State#state.pid]),	
	{next_state, idle, State#state{attackee='undefined'}, timeout(idle)};

attacking({battle_outcome, Result, Other_pid}, State)->
	%%io:format("attacking/2: Pid ~p, Type: ~p, received battle outcome ~p, from ~p~n.",
	%[State#state.pid, State#state.type, Result, Other_pid]),		
	{Next_state, New_state, Timeout}=battle_after_math(Result, State),	
	{next_state, Next_state, New_state, Timeout};

attacking({animation_request, Other_pid}, State)->
	%%io:format("Attacking/2:~p ~p, got animation_request from ~p~n, %%suprresion%%",[State#state.type, State#state.pid, Other_pid]),
	{next_state, idle, State, timeout(idle)};
	
attacking(Event, Data)->	
	unexpected(Event, attacking),
	{next_state, idle, Data, timeout(idle)}.

attacked(timeout, State) -> %Attacked, transition from Idle with timeout=0.
	%%Biggle master graphic update
	biggle_gen_server:fight(State#state.biggle, {State#state.pid, State#state.attacker}),
	%Get results attacker_wins, attackee_wins 
	{Attackee_res,Attacker_res}=decide_fight_resualt(State), %returns atom "attackee_won"/"attacker_won". 
	%send resualt to attacker,
	gen_fsm:send_event(State#state.attacker, {battle_outcome, Attacker_res, State#state.pid}),
	{Next_state, New_state, Timeout}=battle_after_math(Attackee_res, State), %get next state. 
	{next_state, Next_state, New_state, Timeout};

attacked({attacked, Other_pid}, State)->
	%%io:format("attacked/2: Pid ~p, Type: ~p, attacked by ~p.~n%%need suppression%%~n",
	%[State#state.pid, State#state.type, Other_pid]),	
	{next_state, attacked, State, 0};

attacked({animation_request, Other_pid}, State)->
	%%io:format("attacked/2:~p ~p, got animation_request from ~p~n, %%suprresion%%",[State#state.type, State#state.pid, Other_pid]),
	{next_state, attacked, State, timeout()};

attacked(Event, Data)->	
	unexpected(Event, attacked),
	{next_state, idle, Data, timeout(idle)}.

body(timeout, State) ->
	io:format("Body ~p died of decay.~n",[State#state.pid]),
	biggle_gen_server:char_transition(State#state.biggle,{dead}),	
	{next_state, dead, State, 0};

body({battle_outcome, Result, Other_pid}, State)->
	%%io:format("Body/2: Pid ~p, Type: ~p, received battle outcome ~p, from ~p~n, %%Need suprresion%%",	
	%[State#state.pid, State#state.type, Result, Other_pid]),		
	{next_state, body, State, timeout(body)};%TODO: timeout should not restart completly. 

body({attacked, Other_pid}, State)->
	%%io:format("Body/2: Pid ~p, Type: ~p, attacked by ~p.~n %%Need suprresion%%",
	%[State#state.pid, State#state.type, Other_pid]),		
	{next_state, body, State, timeout(body)};%TODO: timeout should not restart completly. 

body({animation_request, Other_pid}, State)->
	%%io:format("Body/2: ~p ~p animated by ww ~p.~n",[State#state.pid, State#state.type, Other_pid]), 
	%Update biggle master. 	
	gen_fsm:send_event(Other_pid, {animation_accepted, State#state.pid}), %respond_to_animation. 		
	biggle_gen_server:char_transition(State#state.biggle,{zombie}),%TODO - Add char transition to zombie
	{next_state, idle, State#state{type=zombie}, timeout(idle)}; 	

body(Event, State) ->
	unexpected(Event, idle),
	{next_state, idle, State, timeout(idle)}.

dead(timeout, State) ->
	%%io:format("~p ~p died. ~n",[State#state.type, State#state.pid]),			
	gen_fsm:send_all_state_event(State#state.pid, stop), 	
	%gen_fsm:stop(State#state.pid),	
	%stop(State#state.pid, i_shall_stop ), 	
	{next_state, dead, State, infinity};

dead(Event, State) ->
	unexpected(Event, idle),
	{next_state, idle, State, timeout(idle)}.

%For white walker
animating(timeout, State) -> %Request to animate not answerd in time. 
	%%io:format("animating/2: ~p ~p, did not get animation reply in time.~n", [State#state.type, State#state.pid]),
	{next_state, waking, State, 0};%Time out is leagal, if for example a body died before being animated.

animating({battle_outcome, _Result, Other_pid}, State)->
	%%io:format("animating/2: ~p ~p, received battle outcome from ~p. %%suprresion%%~n", [State#state.type, State#state.pid, Other_pid]),
	{next_state, animating, State, timeout(idle)};

animating({attacked, Other_pid}, State)->
	%%io:format("animating/2:~p ~p, attacked by ~p.%%suprresion%%~n",[State#state.type, State#state.pid, Other_pid]),		
	{next_state, animating, State, timeout(animating)};

animating({animated, Other_pid}, State)->
	%%io:format("animating/2:~p ~p, animated by ~p.%%suprresion%%~n",[State#state.type, State#state.pid, Other_pid]),		
	{next_state, animating, State, timeout(animating)};

animating({animation_accepted, Other_pid}, State)->
	%%io:format("animating/2:~p accepted animation by ~p ~p. ~n",[Other_pid, State#state.type, State#state.pid]),		
	New_bodies=add_bodies(State#state.bodies, Other_pid),  	
	{next_state, waking, State#state{bodies=New_bodies}, 0}; 

animating(Event, State) ->
	unexpected(Event, idle),
	{next_state, idle, State, timeout(idle)}.
	
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

idle({attacked, _Other_pid}, {From_pid, _Ref}, State) -> 
	%%io:format("In Idle/3: Pid ~p, Type: ~p, attacked by ~p.~n",[State#state.pid, State#state.type, From_pid]),	
	Reply = "Currently cannont be attacked.",
	{reply, Reply, idle, State, timeout(idle)};

idle(Event, _From, State) ->
	unexpected(Event, State), 	
	Reply = ok,
	{reply, Reply, idle, State}.


waking({attacked, _Other_pid}, {From_pid, _Ref}, State) -> %{From_pid, _Ref}
	%%io:format("In waking/3: Pid ~p, Type: ~p, attacked by ~p.~n",[State#state.pid, State#state.type, From_pid]),	
	Reply = "Currently cannont be attacked.",
	{reply, Reply, idle, State, timeout(idle)};

waking(Event, _From, State) ->
	%%io:format("@@@@@Received unreal event!"),
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
%% this handles events in *any* state
handle_event(stop, _AnyState, TheStateData) ->
	%io:format("handle_event **termination**, "),    
	{stop, normal, TheStateData};   %% tell it to stop

handle_event(stopit, _AnyState, _TheStateData) ->
    {stop, i_shall_stop, []};   %% tell it to stop

handle_event(termination, StateName, State) ->
	%io:format("handle_event **termination**, "),    
	{next_state, StateName, State};

handle_event(_Event, StateName, State) ->
	%io:format("handle_event **_Event**, "),     
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

%% This is the finalisation routine - it gets called
%% When we have to stop

terminate(i_shall_stop, _TheStateIwasIn, State) ->
	%io:format("char_gfsm ~p terminating, i shall stop.~n",[State#state.pid]),        
	ok;

terminate(Reason, _StateName, _State) ->
	%io:format("char_gfsm terminating, ~p~n",[Reason]),    
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

%Upon finished battle, calculate next state and variables. return: {Next_state, State, Timeout}
battle_after_math(winner , State)->
	case (State#state.type) of
		warrior ->
			Next_state=idle,
			State_new=State#state{attacker=undefined, attackee=undefined};
		zombie  ->
			Next_state=idle,
			State_new=State#state{attacker=undefined, attackee=undefined};
		white_walker ->
			Next_state=idle,
			State_new=State#state{attacker=undefined, attackee=undefined}
	end,
	{Next_state, State_new, timeout(idle)};

battle_after_math(loser , State)->
	case (State#state.type) of
		warrior ->
			Next_state=body,
			State_new=State#state{type=body, attacker=undefined, attackee=undefined};
  		zombie  ->
			Next_state=dead,
			State_new=State;%#state{attacker=undefined, attackee=undefined},
		white_walker ->
			Next_state=dead,
			State_new=State%#state{attacker=undefined, attackee=undefined};
	end,
	%Update biggle master 
	biggle_gen_server:char_transition(State#state.biggle,{Next_state}),
	{Next_state, State_new, timeout(Next_state)}.
		
%return tuple {Attackee_res,Attacker_res} whith results winner/loser. 
%Recieves State of attackee, and in the record info about attacker. 
decide_fight_resualt(_Attackee_state)-> %TODO - change prob according to types
	{Rand_num, _Seed}=random:uniform_s(os:timestamp()), 	
	case (Rand_num>0.5) of
		false ->
			{winner,loser}; %attackee_won; 
		true  ->
			{loser,winner} %Attacker_won
	end.

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
	{Reply, _New_location}=biggle_gen_server:move_request(State#state.biggle, {State#state.location, Destination, State#state.type}), 
	case (Reply) of
		'ok' ->
			New_state_rec=State#state{location=Destination};
		'denied' ->
			New_state_rec=State
	end, 	 
	{idle, New_state_rec, timeout(idle)};

preform_action(State, {animate, {body, Location, Body_pid} })->
	gen_fsm:send_event(Body_pid, {animation_request, State#state.pid}),
	%io:format("~p ~p in ~p requested animation ~p in ~p.~n",[State#state.type, State#state.pid, State#state.location, Body_pid, Location]),
	{animating, State, timeout(animating)};
	
%Performing an attack is done by changing to attacking mode. 
preform_action(State, {attack, {{Etype, Location1, Other_pid}} }) ->
	%Reply=gen_fsm:sync_send_event(Other_pid, {attacked, Own_pid}, ?attack_timeout),	
	gen_fsm:send_event(Other_pid, {attacked, State#state.pid}),
	%multimedia update TODO
	New_state=State#state{attackee={Etype, Location1, Other_pid}}, 	
	%io:format("Preforming action attack, New state=~p~n",[New_state]),
	{attacking, State#state{attackee={Etype, Location1, Other_pid}}, timeout(attack)}.

%Character chooses where to walk to. 
get_destination(Board_size, Close_chars, {X0,Y0}, Step_size)->
	{Sign_X, Seed }=positive_or_negative(), 
	{Sign_Y, Seed1}=positive_or_negative(Seed), 		
	{RandX, Seed2}=random:uniform_s(Step_size,Seed1), 		
	{RandY, _}=random:uniform_s(Step_size,Seed2),
	{X1,Y1}={X0 + RandX*Sign_X, Y0 + RandY*Sign_Y}, %random close destination. 
	case (X1>0 andalso Y1>0 andalso X1<Board_size andalso Y1<Board_size) of 
		false ->
			get_destination(Board_size, Close_chars, {X0,Y0}, Step_size); 
		true  ->
			Too_close=close_chars(Close_chars, {X1,Y1}, ?min_char_distance), %check if any char closer than min dist. 	
			case Too_close of
				[] ->   %Destination free, return it. 
					{X1,Y1}; 
			_Else -> %Destination occupied, try agian. 
					get_destination(Board_size, Close_chars, {X0,Y0}, Step_size) 
			end	
	end.

choose_action(body, _Board_size, _Close_chars, _Location, _Step_size) ->
		do_nothing; 
choose_action(white_walker, Board_size, Close_chars, Location, Step_size) ->
	Bodies = [ {Etype, Location1, Pid} || {Etype, Location1, Pid}<-Close_chars, Etype=:=body], 
	Enemys = [ {Etype, Location1, Pid} || {Etype, Location1, Pid}<-Close_chars, Etype=:=warrior],	
	case {Bodies,Enemys} of 
		{[],[]}     -> %Move
			Destination=get_destination(Board_size, Close_chars, Location, Step_size),
			{move, {Destination}}; %If no one to attack, move 
		{[],Enemys} -> %Attack	
			Atackee = random:uniform(length(Enemys)),
			{attack, {lists:nth(Atackee, Enemys)}};
		{Bodies, _} -> %Reanimate
			Animatee = random:uniform(length(Bodies)), 
			%io:format("animate: ~p~n",[lists:nth(Animatee, Bodies)]),		
			{animate, lists:nth(Animatee, Bodies)} 
		
	end; 
choose_action(Type, Board_size, Close_chars, Location, Step_size) ->%Warrior or zombie
	%Attack: 
	case Type of 
		warrior ->
			Enemys=[ {Etype, Location1, Pid} || {Etype, Location1, Pid}<-Close_chars, Etype=:=zombie orelse Etype=:=white_walker];
		zombie  ->
			Enemys=[ {Etype, Location1, Pid} || {Etype, Location1, Pid}<-Close_chars, Etype=:=warrior]	
	end, 
	case (Enemys) of	 
		[] ->   %If no one to attack, move 
			Destination=get_destination(Board_size, Close_chars, Location, Step_size),
			{move, {Destination}}; 
		Enemys -> 	
			Atackee = random:uniform(length(Enemys)),
			{attack, {lists:nth(Atackee, Enemys)}}
	end.  
	
%%Timeouts - with random factor
timeout()->
	{Timeout_rand,_}=random:uniform_s(?idle_factor, os:timestamp()),
	Timeout_rand.  	
timeout(body)->
	{Timeout_rand,_}=random:uniform_s(?idle_factor, os:timestamp()),
	?body_timeout + Timeout_rand;  	
timeout(idle)->
	{Timeout_rand,_}=random:uniform_s(?idle_factor, os:timestamp()),
	?idle_period + Timeout_rand;  	
timeout(dead)->
	0;
timeout(animating)->
	{Timeout_rand,_}=random:uniform_s(?idle_factor, os:timestamp()),
	?animation_timeout+Timeout_rand; 
timeout(attack)->
	{Timeout_rand,_}=random:uniform_s(?idle_factor, os:timestamp()),
	?attack_timeout + Timeout_rand.  	
	

positive_or_negative()->
	{Rand, Seed}=random:uniform_s(os:timestamp()), 
	case (Rand>0.5) of
		false ->
			{1,Seed}; 
		true  ->
			{-1, Seed}
	end. 

positive_or_negative(Seed)->
	{Rand, New_seed }=random:uniform_s(Seed), 
	case (Rand>0.5) of
		false ->
			{1, New_seed}; 
		true  ->
			{-1, New_seed}
	end. 

add_bodies(Bodies, Other_pid)->  	
	case Bodies of
		undefined ->
			New_bodies=[{body, Other_pid}];
		_Else     ->
			New_bodies=Bodies++[{body, Other_pid}]
	end, 
	New_bodies. 






