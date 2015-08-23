%% @author bar


-module(mul_server).
-author('BarLesh').

-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(REFRESH_TIME, 80).
-define(STEP, 10).
-define(ABOUT,?wxID_ABOUT).
-define(EXIT,?wxID_EXIT).

-record(state, {frame, panel, nw_img, ww_img, z_img, corpse_img, sparks_img, tree1, tree2, tree3, tree4, tree5, bg, database}). 
  
run(Size)-> spawn(fun()->start(Size) end).
  %% Creeates the window and menus etc.

setFrame(Frame) ->
	MenuBar = wxMenuBar:new(),
	File = wxMenu:new(),
	Help = wxMenu:new(),
	wxMenu:append(Help,?ABOUT,"Statistics of GOT SIM"),
	wxMenu:append(File,?EXIT,"Quit"),
	wxMenuBar:append(MenuBar,File,"&File"),
	wxMenuBar:append(MenuBar,Help,"&Statistics"),
 	wxFrame:setMenuBar(Frame,MenuBar),
	wxFrame:createStatusBar(Frame),
  	wxFrame:setStatusText(Frame,"GOT Simulator"),
  	wxFrame:connect(Frame, command_menu_selected),
  	wxFrame:connect(Frame, close_window).
start(BOARD_SIZE)->
	register(multimedia_server, self()),
	Wx = wx:new(),
	Frame = wxFrame:new(Wx, -1, "GOT SIM", [{size, {BOARD_SIZE, BOARD_SIZE}}]),    
	setFrame(Frame),
	Panel = wxPanel:new(Frame),
	wxFrame:connect(Panel, paint),
	Image = wxImage:new("../Files/nw.png"),
	Image2 = wxImage:new("../Files/ww.png"),
	Image3 = wxImage:new("../Files/z.png"),
	Image4 = wxImage:new("../Files/corps.png"),
	Image5 = wxImage:new("../Files/tree1.png"),
	Image6 = wxImage:new("../Files/tree2.png"),
	Image7 = wxImage:new("../Files/tree3.png"),
	Image8 = wxImage:new("../Files/tree4.png"),
	Image9 = wxImage:new("../Files/tree5.png"),
	Image10 = wxImage:new("../Files/sparks.png"),
	BG=wxBitmap:new(wxImage:new("../Files/snow-bg.png")),
	wxFrame:show(Frame),
	ClientDC = wxClientDC:new(Panel),
	DC=wxBufferedDC:new(ClientDC),
	wxDC:drawBitmap(DC, BG, {0,0}),
	wxBufferedDC:destroy(DC),
	wxClientDC:destroy(ClientDC),
	ETS_table = ets:new(mul_server, []),
	%temp%
	register(refresh_Process, spawn(fun()->refreshSim() end) ),
	State = #state{frame=Frame, panel=Panel, nw_img=Image, ww_img=Image2, 
		z_img=Image3, corpse_img=Image4, sparks_img = Image10, tree1 = Image5, tree2 = Image6,tree3 = Image7,tree4 = Image8, tree5 = Image9, bg=BG, database=ETS_table},
	loop(State).

%evaluate next cordinate to move to (X or Y)
eval(From,From) -> From;
eval(From, To) -> 
	if From<To -> erlang:min(From+?STEP, To);
	true-> erlang:max(From-?STEP, To) end.

%this function is spawned by server gate. it send init msg of "sparks" img (figth), and after 6 intervals of refresh time it sends a delene msg.	
drewFight(Location) ->
	multimedia_server!{init, {sparks, self(), Location}},
	receive
	after ?REFRESH_TIME * 6 -> multimedia_server!{remove, {self()}}
	end.


moveCharacter(_,_,L,L) -> exit(-1);
%this function get a character's type, name and movemnet (from and to). it spawned by server gate ,to send multimedia server new locations update every refresh time [ms] 
moveCharacter(Type, Name,{X_Curr, Y_Curr}, {X_Dest, Y_Dest} )-> 
	receive
	stop -> exit(-1)
	after ?REFRESH_TIME ->  X = eval(X_Curr, X_Dest), Y = eval(Y_Curr, Y_Dest),
				%TODO 
				if (X-X_Curr =:= 0) -> Angle = 0.5*math:pi();
				true -> Angle = math:tan(Y-Y_Curr/ X-X_Curr ) * (math:pi()/180) end, 
				%Angle =0,
				multimedia_server! {update, self(), {Type, Name, {X,Y}, Angle } },
				moveCharacter(Type, Name, {X,Y}, {X_Dest, Y_Dest} )
	end.
%this function updates character's location at 	ETS
updateETS(ETS_table, Type, Name, New_Location, Angle, Updating_Pid )->
	ets:insert(ETS_table,{Name, {Type,New_Location, Angle, Updating_Pid} } ).
	%temp
	%io:format("new location is ~p~n", [ets:lookup(ETS_table, Name)]).

%no more items to show
show(_,_, '$end_of_table',_,_,_,_,_,_,_,_,_,_) -> done;

%this function get Key of next item to show at screen
show(DC,ETS_table, Key, NW_IMG, WW_IMG, Z_IMG, CORPSE_IMG, SPARK_IMG, TREE1, TREE2, TREE3, TREE4, TREE5) -> 
	[{Key, {Type, Pos, Angle,_}}] = ets:lookup(ETS_table, Key),
	case Type of 
		warrior -> Temp =  wxImage:rotate(NW_IMG,Angle, Pos);
		white_walker -> Temp =  wxImage:rotate(WW_IMG,Angle, Pos);
		zombie -> Temp =  wxImage:rotate(Z_IMG,Angle, Pos);
		body -> Temp =  wxImage:rotate(CORPSE_IMG,Angle, Pos);
		sparks -> Temp = wxImage:rotate(SPARK_IMG,Angle, Pos);
		tree1 ->  io:format("at mul_server:show. IMG is:~p~n", [TREE1]), Temp = xwImage:rotate(TREE1, Angle , Pos);
		tree2 -> io:format("at mul_server:show. Angle is:~p~n", [TREE2]), Temp = xwImage:rotate(TREE2, Angle , Pos);
		tree3 -> io:format("at mul_server:show. Angle is:~p~n", [TREE3]), Temp = xwImage:rotate(TREE3, Angle , Pos);
		tree4 -> io:format("at mul_server:show. Angle is:~p~n", [TREE4]), Temp = xwImage:rotate(TREE4, Angle , Pos);
		tree5 -> io:format("at mul_server:show. Angle is:~p~n", [TREE5]), Temp = xwImage:rotate(TREE5, Angle , Pos)
	end,
	Bitmap=wxBitmap:new(Temp),
	wxDC:drawBitmap(DC, Bitmap, Pos),
	wxImage:destroy(Temp),
	wxBitmap:destroy(Bitmap),
	show(DC,ETS_table, ets:next(ETS_table, Key), NW_IMG, WW_IMG, Z_IMG, CORPSE_IMG,SPARK_IMG, TREE1, TREE2, TREE3, TREE4, TREE5).
	
%this function refresh screen accoring to ETS 
showSim(State) ->
	ClientDC = wxClientDC:new(State#state.panel),		%at begining of refresh, show background
	DC=wxBufferedDC:new(ClientDC),
	wxDC:drawBitmap(DC, State#state.bg, {0,0}),
	
	show(DC,State#state.database, ets:first(State#state.database), State#state.nw_img, State#state.ww_img,State#state.z_img,State#state.corpse_img,State#state.sparks_img,State#state.tree1,State#state.tree2,State#state.tree3,State#state.tree4,State#state.tree5),
	wxBufferedDC:destroy(DC),
	wxClientDC:destroy(ClientDC).

%this function send multimedia server a show msg to refresah simulator, at defined time intervals. 
refreshSim() ->
	receive
	stop -> exit(-1)
	after ?REFRESH_TIME -> multimedia_server!show
	end, refreshSim().


showResult(Frame, NW, WW, Z) ->
	io:format("Simulation ended!!!!!!!!!!!!!!!!!!!!!!~n~n"),
	if NW =:= 0 -> Text = "Man have lost the buttle";
	true -> Text = "Victory!!!! We are all saved" end,
	D = wxMessageDialog:new (Frame, Text),
	wxMessageDialog:showModal (D). 

%verify that sending process is relevent to the Name it is trying to update.
varify(_,_, []) -> 0;
varify(Pid, _,  [{ _, {_,_,_,Pid} }] ) -> 1;
varify(_, Type, [{_, {Type,_,_,_} }] ) -> 1;
varify(_,_,_)-> 0.

%routine happanes every time multimedia server closed
close_mull_server(State) ->
	refresh_Process!stop,
	wxWindow:close(State#state.database,[]),
	io:format("mul_server stopped~n"),
	%TODO - free simuletions memory 
	exit(-1).
%mailoop of multimedia server.
loop(State)->
	receive
	%initiation of new object
	{init, {Type, Name, Location} } -> updateETS(State#state.database, Type, Name, Location, 0 , none);
	%update object's new location and angle
	{update, Pid, {Type, Name,  New_Location, Angle} } -> Ans = varify(Pid,Type, ets:lookup(State#state.database, Name)),
					case Ans of 
					0 -> ignore;
					1-> updateETS(State#state.database, Type, Name, New_Location, Angle, Pid ) 
					end;
	%destruction of object
	{remove, {Name}}-> ets:delete(State#state.database, Name);
	%end of simulation msg. sent by SERVER GATE
	{sim_ended, NW, WW, Z} -> showResult(State#state.frame, NW, WW, Z);
	%a status msg sent by server gate
	{status_text,A,B,C,D} -> 	io:format("at mul_server:loop A:~p,B:~p, C:~p, D:~p~n", [A,B,C,D]),
			Text="Warrios:"++integer_to_list(A)++"			White Walkers:"++integer_to_list(B)++"			Zombies:"++integer_to_list(C)++"			resorections"++integer_to_list(D),
			wxFrame:setStatusText(State#state.frame,Text);
	%show msg. sent every ~REFRESH TIME
	show -> showSim(State);
	stop -> close_mull_server(State);
	%graphic intarface msgs
	%exit button clicked
	 #wx{id=?EXIT, event=#wxCommand{type=command_menu_selected}} -> io:format("trying to close~n"),
    		 main!stop;
	%statistics button clicked
	#wx{id=?ABOUT, event=#wxCommand{type=command_menu_selected}} -> server_gate!stat_show; 
	% x clicked
	#wx{event=#wxClose{}} -> main!stop
	end,
	loop(State).



