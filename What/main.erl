%this is a module of main process that runs simulation. it ask user for simulation's parameters, and after getting them, it remotly init Admin processes of sun computing systems (Beagleboards)   
-module(main).
-author('BarLesh').
-compile(export_all).

-define(NUM_OF_BIGGLES, 4).
-define(BOARD_SIZE, 1300).
-define(NUM_TREES, 80).
-define(NUM_ROCKES, 30).

%this function start GUI that ssk from user the simulation's parameters.
askForInput() ->
	spawn(fun()-> menu:start() end),
	io:format("at main's askForInput.~n"),
	receive
	Param-> Param
	end.

initServerGate()->  Pid = spawn(fun()->server_gate:start() end), monitor(process,Pid), Pid.

initStatistics(Parameters) -> spawn(fun()-> statistics:start(Parameters) end).
%starts Graphical show of sumulation
initSimGraphics(BOARD_SIZE) -> spawn(fun()-> mul_server:start(BOARD_SIZE) end).

%initiate Admins processes at remote Controllers
initMaster({NW,WW,Z})->
	%biggle_master:start_link(?NUM_OF_BIGGLES),
	%biggle_master:start_biggles(biggle_master, {?BOARD_SIZE, NW,WW,Z, ?NUM_TREES, ?NUM_ROCKES}).
	biggle_gen_server:start_link(),
	biggle_gen_server:create_world(biggle_gen_server, {?BOARD_SIZE, {{0,500},{0,500}}, NW,WW,Z,0,0}).

%his function is called when simulation is stopped. it send killing signal to all Beagleboards's Admins, wait for confermation from all of them, then send killing signal to SimGrahics GUI, and then killes statistics
stop()->
	killAdmins(), 
	killGate(),
	killSimGraphics(),
	killStatistics(),
	exit(-1). 

run() -> spawn(fun()->start() end).
%this function start GUI and ask from user Simulation's parameter.
start() ->
	register(main, self()),
	%get parameters from user
	Parameters = askForInput(),
	initServerGate(),
	Stat_Pid = initStatistics(Parameters),
	initSimGraphics(?BOARD_SIZE),
	Stat_Pid!start,
	initMaster(Parameters),
	%wit for command to end simulation (can come from either simulation GUI window or Terminal
	receive
		check -> io:format("main is ok.~n");
		{stop, multimedia_server} -> 	io:format("Sim stopping because of multimedia server~n"),
						stop();
		stop -> 		io:format("Sim stopping because of command from Terminal~n"),
						stop()
	end,
	start().
	

killAdmins()-> biggle_gen_server:stop(biggle_gen_server,normal).
killGate() -> server_gate!stop.
killSimGraphics()-> multimedia_server!stop.
killStatistics()-> statistics_server!stop.
