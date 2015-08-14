-module(sim).
-author('BarLesh').

-compile(export_all).

start() -> 
	c(main), c(menu), c(server_gate), c(statistics), c(mul_server), c(biggle_gen_server), c(char_gfsm), biggle_gen_server:start_link().
