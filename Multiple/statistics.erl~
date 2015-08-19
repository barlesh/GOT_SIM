-module(statistics).
-compile(export_all).
-author('BarLesh').

%init statistics server. init ETS 
start(Param) -> 
	%register(statistics_server, self()),
	ETS_table = ets:new(statistics, []),
	ets:insert(ETS_table,{nw_created, 0} ),
	ets:insert(ETS_table,{ww_created, 0} ),
	ets:insert(ETS_table,{z_created, 0} ),
	ets:insert(ETS_table,{resorection, 0} ),
	ets:insert(ETS_table,{nw_killed, 0} ),
	ets:insert(ETS_table,{ww_killed, 0} ),
	ets:insert(ETS_table,{z_killed, 0} ),
	ets:insert(ETS_table,{nw, 0} ),
	ets:insert(ETS_table,{ww, 0} ),
	ets:insert(ETS_table,{z, 0} ),
	waitForStart(ETS_table,Param).
%statistics server starts only after all other parts of sumulation initited. save start time
waitForStart(ETS_table,Param)->
	receive
	start -> io:format("statistics got start msg!~n");
	check -> io:format("stats1 is ok.~n"), waitForStart(ETS_table,Param)
	end,
	{_,Start_Time,_}=now(),
	loop(Start_Time,ETS_table,Param).

%this function is called when user want to see statistics. it spawns a GUI (WX) process with statistics parameters
showStats(ETS_table,Start_Time,Param)->
	{A,B,C} = Param,
	[{_,D}|_] =ets:lookup(ETS_table, nw_killed),
	[{_,E}|_] =ets:lookup(ETS_table, ww_killed),
	[{_,F}|_] =ets:lookup(ETS_table, z_killed),
	[{_,G}|_] =ets:lookup(ETS_table, resorection),
	{_,_,T} = now(),
	H = ((Start_Time-T)/10000),
	H1 = trunc(H),
	io:format("at statistics before opening stat_window params are: ~p,~p,~p,~p,~p,~p,~p,~p~n", [A,B,C,D,E,F,G,H1]),
	spawn(fun()-> stat_window:start( {A,B,C,D,E,F,G,H1 })  end) .

%main loop os statistics server
loop(Start_Time,ETS_table,Param) ->
	[{_,A}] = ets:lookup(ETS_table, nw), 
	[{_,B}] = ets:lookup(ETS_table, ww), 
	[{_,C}] = ets:lookup(ETS_table, z), [{_,D}] = ets:lookup(ETS_table,resorection ),
	server_gate! {stats_update, A,B,C,D},	%%multimadia server update of current characters numbers
	receive
	{warrior, created}  ->  [{nw_created, N}] = ets:lookup(ETS_table, nw_created), ets:insert(ETS_table,{nw_created, N+1} ),
				[{nw, M}] = ets:lookup(ETS_table, nw), ets:insert(ETS_table,{nw, M+1} );
	{white_walker, created}-> [{ww_created, N}] = ets:lookup(ETS_table, ww_created),ets:insert(ETS_table,{ww_created,N+1} ),
				[{ww, M}] = ets:lookup(ETS_table, ww), ets:insert(ETS_table,{ww, M+1} );
	{zombie, created}  -> [{z_created, N}] = ets:lookup(ETS_table, z_created), ets:insert(ETS_table,{nw_created, N+1} ),
				[{z, M}] = ets:lookup(ETS_table, z), ets:insert(ETS_table,{z, M+1} );
	{warrior, killed}  -> [{nw_killed, N}] = ets:lookup(ETS_table, nw_killed), ets:insert(ETS_table,{nw_killed, N+1} ),
				[{nw, M}] = ets:lookup(ETS_table, nw), ets:insert(ETS_table,{nw, M-1} ),
				[{_, K}] = ets:lookup(ETS_table, nw_created),
				io:format("warrior killed. num of warrios is:~p. warriors created is:~p~n", [M-1,K]);
	{white_walker, killed}  -> [{ww_killed, N}] = ets:lookup(ETS_table, ww_killed), ets:insert(ETS_table,{ww_killed, N+1} ),
				[{ww, M}] = ets:lookup(ETS_table, ww), ets:insert(ETS_table,{ww,M-1} );
	{zombie, killed}  -> [{z_killed, N}] = ets:lookup(ETS_table, z_killed), ets:insert(ETS_table,{z_killed, N+1} ),
				[{z, M}] = ets:lookup(ETS_table, z), ets:insert(ETS_table,{z, M-1} );
	{resorection}-> [{resorection, N}] = ets:lookup(ETS_table, resorection), ets:insert(ETS_table,{resorection, N+1} );

	check -> io:format("stats2 is ok.~n");
	print -> print(ETS_table);
	stat_show -> showStats(ETS_table, Start_Time,Param);
	stop -> exit(-1)
	end, 
	loop(Start_Time,ETS_table,Param).

print(ETS_table)->
	[{_,A}] = ets:lookup(ETS_table, nw_created), 
	[{_,B}] = ets:lookup(ETS_table, ww_created), 
	[{_,C}] = ets:lookup(ETS_table, z_created), 
	[{_,D}] = ets:lookup(ETS_table, resorection), 
	[{_,E}] = ets:lookup(ETS_table, nw_killed), 
	[{_,F}] = ets:lookup(ETS_table, ww_killed), 
	[{_,G}] = ets:lookup(ETS_table, z_killed),
	[{_,H}] = ets:lookup(ETS_table, nw),
	[{_,I}] = ets:lookup(ETS_table, ww),
	[{_,J}] = ets:lookup(ETS_table, z),
	io:format("warrior created:~p, white_walker created:~p, zombie created~p, resorections:~p~n", [A,B,C,D]),
	io:format("warrior killed:~p, white_walker killed:~p, zombie killed~p, ~n",[E,F,G ]),
	io:format("warrior Alive:~p, white_walker Alive:~p, zombie Alive~p, ~n", [H,I,J ]).


