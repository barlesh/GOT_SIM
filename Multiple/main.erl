%this is a module of main process that runs simulation. it ask user for simulation's parameters, and after getting them, it remotly init Admin processes of sun computing systems (Beagleboards)   
-module(main).
-author('BarLesh').
-compile(export_all).

-define(NUM_OF_BIGGLES, 4).
-define(BOARD_SIZE, 750).
-define(NUM_TREES, 0).
-define(NUM_ROCKES, 0).

%this function start GUI that ssk from user the simulation's parameters.
askForInput() ->
	spawn(fun()-> menu:start() end),
	io:format("at main's askForInput.~n"),
	receive
	Param-> Param
	end.
%initiation of server gate process, and monitoring it.
initServerGate()->  Pid = spawn(fun()->server_gate:start() end), monitor(process,Pid).

%initiation of statistics process, and monitoring it.
initStatistics(Parameters) -> Pid = spawn(fun()-> statistics:start(Parameters) end), 
				register(statistics_server, Pid) , Ref = monitor(process, Pid), {Pid, Ref}.
%starts Graphical show of sumulation
initSimGraphics(BOARD_SIZE) -> Pid = spawn(fun()-> mul_server:start(BOARD_SIZE) end), monitor(process, Pid).

%initiate Admins processes at remote Controllers
initMaster({NW,WW,Z})-> 
	io:format("initiation of master, arams are:~p~n",[{NW,WW,Z}]),
	biggle_master:start_link(?NUM_OF_BIGGLES),
	timer:sleep(500),
	biggle_master:start_biggles({global,biggle_master}, {?BOARD_SIZE, NW,WW,Z, ?NUM_TREES, ?NUM_ROCKES}).
	%biggle_gen_server:start_link(),
	%biggle_gen_server:create_world(biggle_gen_server, {?BOARD_SIZE, {{0,?BOARD_SIZE},{0,?BOARD_SIZE}}, NW,WW,Z,0,0}).

%his function is called when simulation is stopped. it send killing signal to all Beagleboards's Admins, wait for confermation from all of them, then send killing signal to SimGrahics GUI, and then killes statistics
stop()->
	killAdmins(), 
	timer:sleep(500),
	killGate(),
	timer:sleep(500),
	killSimGraphics(),
	timer:sleep(500),
	killStatistics(),
	timer:sleep(500),
	exit(-1). 

run() -> spawn(fun()->start() end).
%this function start GUI and ask from user Simulation's parameter.
start() ->
	register(main, self()),
	%get parameters from user
	Parameters = askForInput(),
	Server_Gate_Monitor = initServerGate(),
	{Stat_Pid, Stat_Monitor} = initStatistics(Parameters),
	Graphic_Monitor = initSimGraphics(?BOARD_SIZE),
	statistics_server!start,
	initMaster(Parameters),
	loop(Parameters, Server_Gate_Monitor, Stat_Monitor, Graphic_Monitor).
	
loop(Parameters, Server_Gate_Monitor, Stat_Monitor, Graphic_Monitor) -> 
	%wit for command to end simulation (can come from either simulation GUI window or Terminal
	receive
		check -> io:format("main is ok.~n");
		%{'DOWN', Server_Gate_Monitor,_,_,_} -> Ref = initServerGate(), 
		%						loop(Parameters, Ref, Stat_Monitor, Graphic_Monitor);
		%{'DOWN', Stat_Monitor,_,_,_} -> Ref = initStatistics(Parameters), 
		%						loop(Parameters,Server_Gate_Monitor, Ref, Graphic_Monitor);
		%{'DOWN', Graphic_Monitor,_,_,_} -> Ref = initSimGraphics(?BOARD_SIZE),
		%					loop(Parameters, Server_Gate_Monitor, Stat_Monitor, Ref);
		stop -> stop()
	end, loop(Parameters, Server_Gate_Monitor, Stat_Monitor, Graphic_Monitor).
	
%killing functions
killAdmins()-> biggle_master:stop({global, biggle_master},c).
killGate() -> server_gate!stop.
killSimGraphics()-> multimedia_server!stop.
killStatistics()-> statistics_server!stop.
