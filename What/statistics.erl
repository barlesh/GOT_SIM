-module(statistics).
-compile(export_all).
-author('BarLesh').

start() -> 
	register(statistics_server, self()),
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
	waitForStart(ETS_table).
waitForStart(ETS_table)->
	receive
	start -> io:format("statistics got start msg!~n");
	check -> io:format("stats1 is ok.~n"), waitForStart(ETS_table)
	end,
	{_,Start_Time,_}=now(),
	loop(Start_Time,ETS_table).

loop(Start_Time,ETS_table) ->
	[{_,A}] = ets:lookup(ETS_table, nw), [{_,B}] = ets:lookup(ETS_table, ww), [{_,C}] = ets:lookup(ETS_table, z), [{_,D}] = ets:lookup(ETS_table,resorection ),
	multimedia_server! {A,B,C,D},	%%multimadia server update of current characters numbers
	io:format("stats inside loop~n"),
	receive
	{warrior, created}  ->  [{nw_created, N}] = ets:lookup(ETS_table, nw_created), ets:insert(ETS_table,{nw_created, N+1} ),
				[{nw, M}] = ets:lookup(ETS_table, nw), ets:insert(ETS_table,{nw, M+1} );
	{white_walker, created}-> [{ww_created, N}] = ets:lookup(ETS_table, ww_created),ets:insert(ETS_table,{ww_created,N+1} ),
				[{ww, M}] = ets:lookup(ETS_table, ww), ets:insert(ETS_table,{ww, M+1} );
	{zombie, created}  -> [{z_created, N}] = ets:lookup(ETS_table, z_created), ets:insert(ETS_table,{nw_created, N+1} ),
				[{z, M}] = ets:lookup(ETS_table, z), ets:insert(ETS_table,{z, M+1} );
	{warrior, killed}  -> [{nw_killed, N}] = ets:lookup(ETS_table, nw_killed), ets:insert(ETS_table,{nw_killed, N+1} ),
				[{nw, M}] = ets:lookup(ETS_table, nw), ets:insert(ETS_table,{nw, erlang:max(M-1, 0)} );
	{white_walker, killed}  -> [{ww_killed, N}] = ets:lookup(ETS_table, ww_killed), ets:insert(ETS_table,{ww_killed, N+1} ),
				[{ww, M}] = ets:lookup(ETS_table, ww), ets:insert(ETS_table,{ww, erlang:max(M-1, 0)} );
	{zombie, killed}  -> [{z_killed, N}] = ets:lookup(ETS_table, z_killed), ets:insert(ETS_table,{z_killed, N+1} ),
				[{z, M}] = ets:lookup(ETS_table, z), ets:insert(ETS_table,{z, erlang:max(M-1, 0)} );
	{resorection}-> [{resorection, N}] = ets:lookup(ETS_table, resorection), ets:insert(ETS_table,{resorection, N+1} );

	{From, stat_request} -> From!sendStats(Start_Time,ETS_table);
	check -> io:format("stats2 is ok.~n");
	print -> print(ETS_table);
	stop -> %TODO - close ets
		exit(-1)
	end, loop(Start_Time,ETS_table).

print(ETS_table)->
	[{_,A}] = ets:lookup(ETS_table, nw_created), 
	[{_,B}] = ets:lookup(ETS_table, ww_created), 
	[{_,C}] = ets:lookup(ETS_table, z_created), 
	[{_,D}] = ets:lookup(ETS_table, resorection), 
	[{_,E}] = ets:lookup(ETS_table, nw_killed), 
	[{_,F}] = ets:lookup(ETS_table, ww_killed), 
	[{_,G}] = ets:lookup(ETS_table, z_killed),
	io:format("warrior created:~p, white_walker created:~p, zombie created~p, resorections:~p~n",
			 [A,B,C,D]),
		io:format("warrior killed:~p, white_walker killed:~p, zombie killed~p, ~n",
			 [E,F,G ]).
%return tuple of indicator (statistics), tuples of couples {category, counter}, time elepsed from start of simulation 
sendStats(Start_Time,ETS_table) ->
	[A|_] =ets:lookup(ETS_table, nw_created),
	[B|_] =ets:lookup(ETS_table, ww_created),
	[C|_] =ets:lookup(ETS_table, z_created),
	[D|_] =ets:lookup(ETS_table, nw_killed),
	[E|_] =ets:lookup(ETS_table, ww_killed),
	[F|_] =ets:lookup(ETS_table, z_killed),
	[G|_] =ets:lookup(ETS_table, resorection),
	{_,NOW,_} = now(),
	{statistics, {nw_created, A}, {ww_created, B}, {z_created, C}, {nw_killed, D}, {ww_killed, E}, {z_killed, F}, {resorection, G} , NOW -Start_Time } .
