%this is a module of main process that runs simulation. it ask user for simulation's parameters, and after getting them, it remotly init Admin processes of sun computing systems (Beagleboards)   

-author('BarLesh').
-module(main).


-export([start/0]).

%this function start GUI that ssk from user the simulation's parameters.
askForInput() ->
	[].



%his function is called when simulation is stopped. it send killing signal to all Beagleboards's Admins, wait for confermation from all of them, then send killing signal to SimGrahics GUI, and then killes statistics
stop()->
	killAdmins(), 
	killSimGraphics(),
	killStatistics(),
	exit(-1). 
	
%this function start GUI and ask from user Simulation's parameter.
start() ->
	register(main, self()),
	%init Statistics Server
	spawn(fun()-> statistics:start() end),
	%get parameters from user
	Parameters = askForInput(),

	initSimGraphics(),
	initAdmins(Parameters),
	receive
		{stop, multimedia_server} -> 	io:format("Sim stopping because of multimedia server~"),
						stop();
		{stop, Terminal} -> 		io:format("Sim stopping because of command from Terminal~"),
						stop();
	end.
	

killAdmins()-> [].
killSimGraphics()-> mul_server!stop.
killStatistics()-> statistics_server!stop.
