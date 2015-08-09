
-module(menu).
-compile(export_all).
-author('BarLesh').
-include_lib("wx/include/wx.hrl").
 
-define(MIN_NW,(10)).
-define(MIN_WW,(3)).
-define(MIN_ZOMBIE,(15)).
-define(MAX_NW,(70)).
-define(MAX_WW,(15)).
-define(MAX_ZOMBIE,(100)).


%% Note these changes from wxcd04.erl
%% 1) gave T1001 a default value of 10
%% 2) add some space the controls
%% 3) created OuterSizer, put MainSizer in it, with space around
 
exitMenu(Frame) -> %TODO
	wxWindow:destroy(Frame),
	exit(-1).

start() ->
    State = make_window(),
    loop (State).
 
make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "GOT Sim Menu", [{size,{900, 400}}]),
    Panel  = wxPanel:new(Frame),
    Canvas = wxPanel:new(Panel, []),
 
%% create widgets
%% the order entered here does not control appearance
    T1001 = wxTextCtrl:new(Panel, 1001,[{value, "40"}]), %set default value
    T1002 = wxTextCtrl:new(Panel, 1001,[{value, "5"}]), %set default value
    T1003 = wxTextCtrl:new(Panel, 1001,[{value, "70"}]), %set default value
    B101  = wxButton:new(Panel, 101, [{label, "&Start Simulation"}]),
    B102  = wxButton:new(Panel, ?wxID_EXIT, [{label, "C&ancel"}]),
 
%%You can create sizers before or after the widgets that will go into them, but
%%the widgets have to exist before they are added to sizer.
    OuterSizer   = wxBoxSizer:new(?wxHORIZONTAL),
    ImageSizer   = wxBoxSizer:new(?wxVERTICAL),
    MainSizer    = wxBoxSizer:new(?wxVERTICAL),
    NW_InputSizer  = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Number of Night Watch Warriors"}]),
    WW_InputSizer  = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Number of White Walkers"}]),
    Z_InputSizer  = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Number of Zombies"}]),
    ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
 
%% Note that the widget is added using the VARIABLE, not the ID.
%% The order they are added here controls appearance.
 
    wxSizer:add(NW_InputSizer, T1001, []),
    wxSizer:add(NW_InputSizer, 120, 0,[]),  %see note in tutorial text
 
    wxSizer:add(WW_InputSizer, T1002, []),
    wxSizer:add(WW_InputSizer, 120, 0,[]),  %see note in tutorial text

    wxSizer:add(Z_InputSizer, T1003, []),
    wxSizer:add(Z_InputSizer, 120, 0,[]),  %see note in tutorial text


    wxSizer:addSpacer(MainSizer, 90),  %spacer
    wxSizer:add(MainSizer, NW_InputSizer,[]),
    wxSizer:addSpacer(MainSizer, 5),  %spacer
    
    wxSizer:addSpacer(MainSizer, 10),  %spacer
    wxSizer:add(MainSizer, WW_InputSizer,[]),
    wxSizer:addSpacer(MainSizer, 5),  %spacer

    wxSizer:addSpacer(MainSizer, 10),  %spacer
    wxSizer:add(MainSizer, Z_InputSizer,[]),
    wxSizer:addSpacer(MainSizer, 5),  %spacer
 
 
    wxSizer:add(ButtonSizer, B101,  []),
    wxSizer:add(ButtonSizer, B102,  []),
    wxSizer:add(MainSizer, ButtonSizer, []),
 
    wxSizer:addSpacer(OuterSizer, 20), % spacer
    wxSizer:add(OuterSizer, MainSizer, []),

    %set image
    
    wxPanel:connect(Canvas, paint, []),
    wxPanel:connect(Canvas, size),
    wxSizer:add(ImageSizer, Canvas, []),
    wxSizer:add( OuterSizer,ImageSizer, []),

    DC = wxPaintDC:new(Canvas),
    BG=wxBitmap:new(wxImage:new("/home/barlesh/Erworkspace/FinalProject/bar/snow-bg.png")),
    MemoryDC = wxMemoryDC:new(BG),
     wxDC:clear(DC),
    wxDC:drawBitmap(DC, BG, {0,0}),

    CDC = wxWindowDC:new(Canvas),
    wxDC:blit(CDC, {0,0},
	      {wxBitmap:getWidth(BG), wxBitmap:getHeight(BG)},
	      MemoryDC, {0,0}),    
    wxWindowDC:destroy(CDC),
    wxMemoryDC:destroy(MemoryDC),


    %ClientDC = wxClientDC:new(Canvas),
    %DC=wxBufferedDC:new(ClientDC), 
    %wxDC:drawBitmap(DC, BG, {0,0}),
    %wxBufferedDC:destroy(DC),
    %wxClientDC:destroy(ClientDC),

%% Now 'set' OuterSizer into the Panel
    wxPanel:setSizer(Panel, OuterSizer),
 
     wxFrame:connect( Frame, close_window),
    wxPanel:connect(Panel, command_button_clicked),

    wxFrame:show(Frame),
    {Frame, T1001,T1002,T1003}.  %the return value will go here, soon
 
loop(State) -> 
	receive 
	#wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
		{Frame, T1001,T1002,T1003} = State,
		NW=list_to_integer(wxTextCtrl:getValue(T1001)),
		WW=list_to_integer(wxTextCtrl:getValue(T1002)),
		Z=list_to_integer(wxTextCtrl:getValue(T1003)),	
		case checkParams(NW,WW,Z) of ok-> main!{NW,WW,Z}, exitMenu(Frame); error -> showError(Frame) end;

	#wx{event=#wxClose{}} ->
            io:format("~p Closing window ~n",[self()]), %optional, goes to shell
	{Frame, _,_,_} = State,
        exitMenu(Frame);

	           %this message is sent when the exit button is clicked.
	#wx{id = ?wxID_EXIT, event=#wxCommand{type = command_button_clicked} } ->
            io:format("~p Closing window ~n",[self()]), 
	{Frame, _,_,_} = State,
        exitMenu(Frame)
	end,
	loop(State).

%this function pop notfication about wrong input (parameters)
showError(Frame)->
	D = wxMessageDialog:new (Frame, "Wrong Parameters. Try again"),
	wxMessageDialog:showModal (D). 
	
%returns true if parameter given is at valid range
checkParams(NW,WW,Z)-> 	
	io:format("params ar check Params are ~p,~p,~p~n", [NW,WW,Z]),
	ok.
	%TODO - fix!	
	%case NW of 
	%	NW1 when NW1<?MIN_NW -> error;
	%	NW2 when NW2>?MAX_NW -> error;
	%	Else -> 
	%		case WW of 
	%			WW1 when WW1<?MIN_WW -> error;
	%			WW2 when WW2>?MAX_WW -> error;	
	%			Else -> 
	%				case Z of 
	%				Z1 when Z1<?MIN_ZOMBIE -> error;
	%				Z2 when Z2>?MAX_ZOMBIE -> error;
	%				Else -> ok
	%				end
	%		end
	%end.


