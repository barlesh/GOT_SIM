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
	receive
	start -> {_,Start,_}=now(),
	end,
	loop(Start).

loop(Start) ->
	receive
	{nw, created}  -> [{nw_created, N}] = ets:lookup(ETS_table, nw_created), ets:insert(ETS_table,{nw_created, N+1} );
	{ww, created}  -> [{ww_created, N}] = ets:lookup(ETS_table, ww_created), ets:insert(ETS_table,{ww_created, N+1} );
	{z, created}  -> [{nw_created, N}] = ets:lookup(ETS_table, z_created), ets:insert(ETS_table,{nw_created, N+1} );
	{nw, killed}  -> [{nw_killed, N}] = ets:lookup(ETS_table, nw_killed), ets:insert(ETS_table,{nw_killed, N+1} );
	{ww, killed}  -> [{ww_killed, N}] = ets:lookup(ETS_table, ww_killed), ets:insert(ETS_table,{ww_killed, N+1} );
	{z, killed}  -> [{z_killed, N}] = ets:lookup(ETS_table, z_killed), ets:insert(ETS_table,{z_killed, N+1} );
	{resorection}-> [{resorection, N}] = ets:lookup(ETS_table, resorection), ets:insert(ETS_table,{resorection, N+1} );

	{From, stat_request} -> From!sendStats(Start);
	stop -> %TODO - close ets
		exit(-1).
%return tuple of indicator (statistics), tuples of couples {category, counter}, time elepsed from start of simulation 
sendStats(Start) ->
	[A|_] =ets:lookup(ETS_table, nw_created),
	[B|_] =ets:lookup(ETS_table, ww_created),
	[C|_] =ets:lookup(ETS_table, z_created),
	[D|_] =ets:lookup(ETS_table, nw_killed),
	[E|_] =ets:lookup(ETS_table, ww_killed),
	[F|_] =ets:lookup(ETS_table, z_killed),
	[G|_] =ets:lookup(ETS_table, resorection),
	{_,NOW,_} = now(),
	{statistics, {nw_created, A}, {ww_created, B}, {z_created, C}, {nw_killed, D}, {ww_killed, E}, {z_killed, F}, {resorection, G} , Now -Start } .
