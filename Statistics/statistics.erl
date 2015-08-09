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
	{_,Start,_}=now(),
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

	{From, stat_request} -> From!sendStats();
	stop -> %TODO - close ets
		exit(-1).

sendStats() ->
	[].
