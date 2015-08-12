-module(server_gate).
-compile(export_all).
-author('BarLesh').

start() ->
	register(server_gate, self() ),
	loop().
loop()->
	receive
	{born, {Type, Name, Location} } -> multimedia_server!{update, {Type, Name,  Location, 0} },
					statistics_server!{Type, created};
	{death, {Type, Name}} -> multimedia_server!{remove, {Name} },
			statistics_server!{Type, killed};
	{transition, {Original_Type, Trans_Type, Name, Location} } -> multimedia_server!
								{update, {Trans_Type, Name,  Location, 0} }, 
								case Original_Type of 
								warrior -> statistics_server!{warrior, killed};
								body -> statistics_server!{zombie, created}
								end;
	{movement, {Type, Name, {X1,Y1}, {X2,Y2} }  } -> spawn(fun()-> mul_server:moveCharacter(Type, Name, 
									{trunc(X1), trunc(Y1)}, {trunc(X2), trunc(Y2)} ) end);	
	{fight, {{Type1, Name1}, {Type2, Name2}, Location}} -> [];%TODO
	{won_figth, {Type, Name, New_Location}} -> []%updateETS(State#state.database, Type, Name, New_Location, 0 )	


	end, loop().
