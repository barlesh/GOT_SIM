-module(stat_window).
-compile(export_all).
-author('BarLesh').
-include_lib("wx/include/wx.hrl").

-define(EXIT,?wxID_EXIT).

-record(parameters, {frame}).

make_window(Param) ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "GOT Sim Statistics", [{size,{600, 200}}]),
    Panel  = wxPanel:new(Frame),
    %% create widgets
%% the order entered here does not control appearance
    B101  = wxButton:new(Panel, ?wxID_EXIT, [{label, "C&lose"}]),
 
%%You can create sizers before or after the widgets that will go into them, but
%%the widgets have to exist before they are added to sizer.
    OuterSizer   = wxBoxSizer:new(?wxHORIZONTAL),
    ImageSizer   = wxBoxSizer:new(?wxVERTICAL),
   %ImageSizer   = wxStaticBoxSizer:new(?wxVERTICAL, Panel, []),
    MainSizer    = wxStaticBoxSizer:new(?wxVERTICAL,Panel, []),
    TextSizer    = wxStaticBoxSizer:new(?wxVERTICAL,Panel, []),
    ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
 
%% Note that the widget is added using the VARIABLE, not the ID.
%% The order they are added here controls appearance.
 
  {W_CREATED, WW_CREATED, Z_CREATED, W_KILLED, WW_KILLED, Z_KILLED, RESORECTION, TIME_ELEPSED} = Param,
  T1 = "Warriors Created:" ++ integer_to_list(W_CREATED),
  T2 = "White Walkers Created:" ++ integer_to_list(WW_CREATED),
  T3 = "Zombies Created:" ++ integer_to_list(Z_CREATED),
  T4 = "Warriors Killed:" ++ integer_to_list(W_KILLED),
  T5 = "White Walkers Killed:" ++ integer_to_list(WW_KILLED),
  T6 = "Zombies Killed:" ++ integer_to_list(Z_KILLED),
  T7 = "num of resorections:" ++ integer_to_list(RESORECTION),
  T8 = "Time Elepsed:" ++ integer_to_list(TIME_ELEPSED),
  Texts = [wxStaticText:new(Panel, 1, T1, []),
	wxStaticText:new(Panel, 2, T2 ,[]),
	wxStaticText:new(Panel, 3, T3 ,[]), 
	wxStaticText:new(Panel, 4, T4,[]),
	wxStaticText:new(Panel, 5, T5,[]),
	wxStaticText:new(Panel, 6, T6,[]),
	wxStaticText:new(Panel, 7, T7,[]),
	wxStaticText:new(Panel, 8, T8,[])],

    [ wxSizer:add(TextSizer, Text, []) || Text <- Texts] ,
    wxSizer:add(MainSizer, TextSizer, []),
    wxSizer:addSpacer(OuterSizer, 30), % space

    wxSizer:add(ButtonSizer, B101,  []),
    wxSizer:add(MainSizer, ButtonSizer, []),
 
    wxSizer:addSpacer(OuterSizer, 20), % spacer
    wxSizer:add(OuterSizer, MainSizer, []),
    wxSizer:addSpacer(OuterSizer, 60), % spacer

    %set image
    Canvas = wxPanel:new(Panel, [{size, {270, 170}}]),

    wxPanel:connect(Canvas, paint, []),
    %wxPanel:connect(Canvas, size),
    wxSizer:addSpacer(ImageSizer, 15), % spacer
    wxSizer:add(ImageSizer, Canvas, []),
    wxSizer:add( OuterSizer,ImageSizer, []),


%% Now 'set' OuterSizer into the Panel
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
    MyPid = self(),
    register(stat_window,MyPid),
    State = make_window(Param),
    loop (State).

loop(State) -> 
	receive 
	#wx{event=#wxClose{}} ->
            io:format("~p Closing window ~n",[self()]), %optional, goes to shell
        exitStatWindow(State#parameters.frame);

	           %this message is sent when the exit button is clicked.
	#wx{id = ?wxID_EXIT, event=#wxCommand{type = command_button_clicked} } ->
            io:format("~p Closing window ~n",[self()]), 
        exitStatWindow(State#parameters.frame)
	end,
	loop(State).


