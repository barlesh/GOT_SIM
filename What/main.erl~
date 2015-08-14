%this is a module of main process that runs simulation. it ask user for simulation's parameters, and after getting them, it remotly init Admin processes of sun computing systems (Beagleboards)   
-module(main).
-author('BarLesh').
-compile(export_all).

%this function start GUI that ssk from user the simulation's parameters.
askForInput() ->
	spawn(fun()-> menu:start() end),
	io:format("at main's askForInput.~n"),
	receive
	Param-> Param
	end.

initServerGate()->spawn(fun()->server_gate:start() end).

initStatistics() -> spawn(fun()-> statistics:start() end).
%starts Graphical show of sumulation
initSimGraphics() -> spawn(fun()-> mul_server:start() end).

%initiate Admins processes at remote Controllers
initAdmins(Parameters)->
	%rpc mutherfucker
	[].

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
	%init Statistics Server
	%spawn(fun()-> statistics:start() end),
	%get parameters from user
	Parameters = askForInput(),
	io:format("Params are ~p~n", [Parameters]),
	initServerGate(),
	Stat_Pid = initStatistics(),
	initSimGraphics(),
	initAdmins(Parameters),
	Stat_Pid!start,
	io:format("main pass seng to stats~n"),
	%wit for command to end simulation (can come from either simulation GUI window or Terminal
	receive
		check -> io:format("main is ok.~n");
		{stop, multimedia_server} -> 	io:format("Sim stopping because of multimedia server~n"),
						stop();
		stop -> 		io:format("Sim stopping because of command from Terminal~n"),
						stop()
	end,
	start().
	

killAdmins()-> [].
killGate() -> server_gate!stop.
killSimGraphics()-> multimedia_server!stop.
killStatistics()-> statistics_server!stop.
