-module(stat_window).
-compile(export_all).
-author('BarLesh').
-include_lib("wx/include/wx.hrl").

-define(EXIT,?wxID_EXIT).

-record(parameters, {frame}).

make_window(Param) ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "GOT Sim Statistics", [{size,{850, 370}}]),
    Panel  = wxPanel:new(Frame),
    %% create widgets
    B101  = wxButton:new(Panel, ?wxID_EXIT, [{label, "C&lose"}]),
 

    OuterSizer   = wxBoxSizer:new(?wxHORIZONTAL),
    ImageSizer   = wxBoxSizer:new(?wxVERTICAL),
    MainSizer    = wxStaticBoxSizer:new(?wxVERTICAL,Panel, []),
    TextSizer    = wxStaticBoxSizer:new(?wxVERTICAL,Panel, []),
    ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
 
 io:format("Param is:~p~n", [Param]),

  {W_CREATED, WW_CREATED,Z_CREATED,W_KILLED,WW_KILLED,Z_KILLED,RESORECTION,TIME_ELEPSED } = Param,

  T1 = "Warriors Created:" ++ integer_to_list(W_CREATED),
  SPACE = " ",
  T2 = "White Walkers Created:" ++ integer_to_list(WW_CREATED),
  T3 = "Zombies Created:" ++ integer_to_list(Z_CREATED),
  T4 = "Warriors Killed:" ++ integer_to_list(W_KILLED),
  T5 = "White Walkers Killed:" ++ integer_to_list(WW_KILLED),
  T6 = "Zombies Killed:" ++ integer_to_list(Z_KILLED),
  T7 = "num of resorections:" ++ integer_to_list(RESORECTION),
  T8 = "Time Elepsed:" ++ integer_to_list(TIME_ELEPSED),
  Texts = [wxStaticText:new(Panel, 1, T1, []),
	wxStaticText:new(Panel, 2, SPACE ,[]),
	wxStaticText:new(Panel, 3, T2 ,[]),
	wxStaticText:new(Panel, 4, SPACE ,[]),
	wxStaticText:new(Panel, 5, T3 ,[]), 
	wxStaticText:new(Panel, 6, SPACE ,[]),
	wxStaticText:new(Panel, 7, T4,[]),
	wxStaticText:new(Panel, 8, SPACE ,[]),
	wxStaticText:new(Panel, 9, T5,[]),
	wxStaticText:new(Panel, 10, SPACE ,[]),
	wxStaticText:new(Panel, 11, T6,[]),
	wxStaticText:new(Panel, 12, SPACE ,[]),
	wxStaticText:new(Panel, 13, T7,[]),
	wxStaticText:new(Panel, 14, SPACE ,[]),
	wxStaticText:new(Panel, 15, T8,[])],

    [ wxSizer:add(TextSizer, Text, []) || Text <- Texts] ,
    wxSizer:add(MainSizer, TextSizer, []),
    wxSizer:addSpacer(OuterSizer, 7), % space

    wxSizer:add(ButtonSizer, B101,  []),
    wxSizer:add(MainSizer, ButtonSizer, []),
 
    wxSizer:addSpacer(OuterSizer, 20), % spacer
    wxSizer:add(OuterSizer, MainSizer, []),
    wxSizer:addSpacer(OuterSizer, 60), % spacer

    %set image
    Canvas = wxPanel:new(Panel, [{size, {500, 370}}]),

    wxPanel:connect(Canvas, paint, []),
    wxSizer:addSpacer(ImageSizer, 15), % spacer
    wxSizer:add(ImageSizer, Canvas, []),
    wxSizer:add( OuterSizer,ImageSizer, []),

    wxPanel:setSizer(Panel, OuterSizer),
 
     wxFrame:connect( Frame, close_window),
    wxPanel:connect(Panel, command_button_clicked),

    wxFrame:show(Frame),

    IMG=wxImage:new("/home/barlesh/Erworkspace/FinalProject/GOT_SIM/Files/PAMELA.png"),
    ClientDC = wxClientDC:new(Canvas),
    Bitmap = wxBitmap:new(IMG),
    wxDC:drawBitmap(ClientDC, Bitmap, {0,0}),
    wxBitmap:destroy(Bitmap),
    wxClientDC:destroy(ClientDC),
    #parameters{frame=Frame}.


exitStatWindow(Frame) -> %TODO
	wxWindow:destroy(Frame),
	exit(-1).

start(Param) ->
    %MyPid = self(),
    %register(stat_window,MyPid),
    State = make_window(Param),
    loop (State).

loop(State) -> 
	receive 
	#wx{event=#wxClose{}} -> exitStatWindow(State#parameters.frame);
	           %this message is sent when the exit button is clicked.
	#wx{id = ?wxID_EXIT, event=#wxCommand{type = command_button_clicked} } -> exitStatWindow(State#parameters.frame)
	end,
	loop(State).


