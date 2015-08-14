%% @author bar


-module(mul_server).
-author('BarLesh').

-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(max_x,(1000)).
-define(max_y,(1000)).
-define(REFRESH_TIME, 80).
-define(STEP, 10).
-define(ABOUT,?wxID_ABOUT).
-define(EXIT,?wxID_EXIT).

-record(state, {frame, panel, nw_img, ww_img, z_img, corpse_img, bg, database}). 
  
run()-> spawn(fun()->start() end).
  %% Creeates the window and menus etc.

setFrame(Frame) ->
	MenuBar = wxMenuBar:new(),
	File = wxMenu:new(),
	Help = wxMenu:new(),
	wxMenu:append(Help,?ABOUT,"About GOT SIM"),
	wxMenu:append(File,?EXIT,"Quit"),
	wxMenuBar:append(MenuBar,File,"&File"),
	wxMenuBar:append(MenuBar,Help,"&Help"),
 	wxFrame:setMenuBar(Frame,MenuBar),
	wxFrame:createStatusBar(Frame),
  	wxFrame:setStatusText(Frame,"Welcome to wxErlang"),
  	wxFrame:connect(Frame, command_menu_selected),
  	wxFrame:connect(Frame, close_window).
start()->
	register(multimedia_server, self()),
	Wx = wx:new(),
	Frame = wxFrame:new(Wx, -1, "GOT SIM", [{size, {?max_x, ?max_y}}]),    
	setFrame(Frame),
	Panel = wxPanel:new(Frame),
	wxFrame:connect(Panel, paint),
	Image = wxImage:new("/home/barlesh/Erworkspace/FinalProject/bar/nw.png"),
	Image2 = wxImage:new("/home/barlesh/Erworkspace/FinalProject/bar/ww.png"),
	Image3 = wxImage:new("/home/barlesh/Erworkspace/FinalProject/bar/z.png"),
	Image4 = wxImage:new("/home/barlesh/Erworkspace/FinalProject/snir/car.png"),
	BG=wxBitmap:new(wxImage:new("/home/barlesh/Erworkspace/FinalProject/bar/snow-bg.png")),
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
		z_img=Image3, corpse_img=Image4, bg=BG, database=ETS_table},
	loop(State).

eval(From,From) -> From;
eval(From, To) -> 
	if From<To -> erlang:min(From+?STEP, To);
	true-> erlang:max(From-?STEP, To) end.
		
moveCharacter(_,_,L,L) -> exit(-1);
%this function get a character's type, name and movemnet (from and to). it spawns a new process to sent main process new locations update every refresh time [ms] 
moveCharacter(Type, Name,{X_Curr, Y_Curr}, {X_Dest, Y_Dest} )-> 
	receive
	stop -> exit(-1)
	after ?REFRESH_TIME ->  X = eval(X_Curr, X_Dest), Y = eval(Y_Curr, Y_Dest),
				%TODO 
				%if (X-X_Curr =:= 0) -> Angle = 0.5*math:pi();
				%true -> Angle = math:tan(Y-Y_Curr, X-X_Curr ) * (mathi:pi()/180) end, 
				Angle =0,
				multimedia_server! {update, {Type, Name, {X,Y}, Angle } },
				moveCharacter(Type, Name, {X,Y}, {X_Dest, Y_Dest} )
	end.
%this function updates character's location at 	ETS
updateETS(ETS_table, Type, Name, New_Location, Angle )->
	ets:insert(ETS_table,{Name, {Type,New_Location, Angle} } ).
	%temp
	%io:format("new location is ~p~n", [ets:lookup(ETS_table, Name)]).

%no more items to show
show(_,_, '$end_of_table',_,_,_,_) -> done;

%this function get Key of next item to show at screen
show(DC,ETS_table, Key, NW_IMG, WW_IMG, Z_IMG, CORPSE_IMG) -> 
	[{Key, {Type, Pos, Angle}}] = ets:lookup(ETS_table, Key),
	case Type of 
		warrior -> Temp =  wxImage:rotate(NW_IMG,Angle, Pos);
		white_walker -> Temp =  wxImage:rotate(WW_IMG,Angle, Pos);
		zombie -> Temp =  wxImage:rotate(Z_IMG,Angle, Pos);
		body -> Temp =  wxImage:rotate(CORPSE_IMG,Angle, Pos)
	end,
	Bitmap=wxBitmap:new(Temp),
	wxDC:drawBitmap(DC, Bitmap, Pos),
	wxImage:destroy(Temp),
	wxBitmap:destroy(Bitmap),
	show(DC,ETS_table, ets:next(ETS_table, Key), NW_IMG, WW_IMG, Z_IMG, CORPSE_IMG).
	
%this function refresh screen accoring to ETS 
showSim(State) ->
	ClientDC = wxClientDC:new(State#state.panel),		%at begining of refresh, show background
	DC=wxBufferedDC:new(ClientDC),
	wxDC:drawBitmap(DC, State#state.bg, {0,0}),
	
	show(DC,State#state.database, ets:first(State#state.database), State#state.nw_img, State#state.ww_img, State#state.z_img, State#state.corpse_img),
	wxBufferedDC:destroy(DC),
	wxClientDC:destroy(ClientDC).

%this function send multimedia server a show msg to refresah simulator, at defined time intervals
refreshSim() ->
	receive
	stop -> exit(-1)
	after ?REFRESH_TIME -> multimedia_server!show
	end, refreshSim().

%routine happanes every time multimedia server closed
close_mull_server() ->
	refresh_Process!stop,
	%TODO - free simuletions memory 
	exit(-1).
%this function wait for inner msg (3 types of msgs), or user request from GUI:
%a. (inner) show - comes every ~10 [ms] and replace current screen with new one with ets's info
%b. (inner) movement - create new process whice will sent constantly new locations (updates) for a given chracter
%c. (inner) update - a new location update for a given character (chracter name is an atom - key at ets)
%d. (user) exit
%e. (user) open ABOUT window
%f. (user) statistics window TODO
loop(State)->
	receive
	{init, {Type, Name, Location} } -> updateETS(State#state.database, Type, Name, Location, 0 );
	{update, {Type, Name,  New_Location, Angle} } -> L = ets:lookup(State#state.database, Name),
					if length(L) =:= 0 -> ignored;
					true -> updateETS(State#state.database, Type, Name, New_Location, Angle ) end; 
	{remove, {Name}}-> ets:delete(State#state.database, Name);
	{A,B,C,D} -> Text="Warrios:"++integer_to_list(A)++"			Zombies:"++integer_to_list(B)++"			White Walkers:"++integer_to_list(C)++"			resorections"++integer_to_list(D),
	wxFrame:setStatusText(State#state.frame,Text);
	show -> showSim(State);
	stop -> close_mull_server();
	%user interface msg
	 #wx{id=?EXIT, event=#wxCommand{type=command_menu_selected}} -> io:format("trying to close~n"),
    		wxWindow:close(State#state.database,[]), close_mull_server();
	#wx{id=?ABOUT, event=#wxCommand{type=command_menu_selected}} -> io:format("OPEN ABOUT~n"); %TODO
	#wx{event=#wxClose{}} -> close_mull_server()
	end,
	loop(State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TEMPURARY FUNCTION FOR CHECKING PROGRAM%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move(Key, Type) ->
	random:seed(now()),
	X1 =  random:uniform(700),
	X2 =  random:uniform(700),
	Y1 =  random:uniform(700),
	Y2 =  random:uniform(700),
	case Type of
	warrior-> server_gate!{movement, {warrior, Key, {X1,Y1}, {X2,Y2} } };
	white_walker-> server_gate!{movement, {white_walker, Key, {X1,Y1}, {X2,Y2} } };
	zombie -> server_gate!{movement, {zombie, Key, {X1,Y1}, {X2,Y2} } }
	end.
	
test(N) -> 
	[ move( "nw"++integer_to_list(X), warrior) || X<-lists:seq(1,trunc(N/3))],
	timer:sleep(17),
	[ move( "ww"++integer_to_list(X), white_walker) || X<-lists:seq(1,trunc(N/3))],
	timer:sleep(17),
	[ move( "z"++integer_to_list(X), zombie) || X<-lists:seq(1,trunc(N/3) )].

