
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

-record(parameters, {frame,canvas, nw, ww, z}).

%% Note these changes from wxcd04.erl
%% 1) gave T1001 a default value of 10
%% 2) add some space the controls
%% 3) created OuterSizer, put MainSizer in it, with space around
 
exitMenu(Frame) -> %TODO
	wxWindow:destroy(Frame),
	exit(-1).

refreshIMGthread()->
	receive
	{'DOWN', Ref, process, Pid2, Reason} -> exit(-1)
	after 7000 -> menu!changIMG
	end,
	refreshIMGthread().

refreshIMGthread(Menu_Pid)->
	erlang:monitor(process, Menu_Pid),
	refreshIMGthread().

start1() ->
    MyPid = self(),
    register(menu,MyPid),
    spawn(fun()-> refreshIMGthread(MyPid) end),
    State = make_window(),
    loop (State).
 
make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "GOT Sim Menu", [{size,{900, 600}}]),
    Panel  = wxPanel:new(Frame),

 
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
   %ImageSizer   = wxStaticBoxSizer:new(?wxVERTICAL, Panel, []),
    MainSizer    = wxBoxSizer:new(?wxVERTICAL),
    NW_InputSizer  = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "# of Night Watch Warriors"}]),
    WW_InputSizer  = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "# of White Walkers"}]),
    Z_InputSizer  = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "# of Zombies"}]),
    ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
 
%% Note that the widget is added using the VARIABLE, not the ID.
%% The order they are added here controls appearance.
 
    wxSizer:add(NW_InputSizer, T1001, []),
    wxSizer:add(NW_InputSizer, 120, 0,[]),  %see note in tutorial text
 
    wxSizer:add(WW_InputSizer, T1002, []),
    wxSizer:add(WW_InputSizer, 120, 0,[]),  %see note in tutorial text

    wxSizer:add(Z_InputSizer, T1003, []),
    wxSizer:add(Z_InputSizer, 120, 0,[]),  %see note in tutorial text


    wxSizer:addSpacer(MainSizer, 15),  %spacer
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
    wxSizer:addSpacer(OuterSizer, 60), % spacer

    %set image
    Canvas = wxPanel:new(Panel, [{size, {570, 570}}]),

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
    #parameters{frame=Frame,canvas=Canvas, nw=T1001, ww=T1002, z=T1003}.
 
loop(State) -> 
	
	receive 
	#wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
		NW=list_to_integer(wxTextCtrl:getValue(State#parameters.nw)),
		WW=list_to_integer(wxTextCtrl:getValue(State#parameters.ww)),
		Z=list_to_integer(wxTextCtrl:getValue(State#parameters.z)),	
		case checkParams(NW,WW,Z) of ok-> main!{NW,WW,Z}, exitMenu(State#parameters.frame); error -> showError(State#parameters.frame) end;

	#wx{event=#wxClose{}} ->
            io:format("~p Closing window ~n",[self()]), %optional, goes to shell
        exitMenu(State#parameters.frame);

	           %this message is sent when the exit button is clicked.
	#wx{id = ?wxID_EXIT, event=#wxCommand{type = command_button_clicked} } ->
            io:format("~p Closing window ~n",[self()]), 
        exitMenu(State#parameters.frame);
	changIMG -> changeIMG(State#parameters.canvas)
	end,
	loop(State).

%this function pop notfication about wrong input (parameters)
showError(Frame)->
	D = wxMessageDialog:new (Frame, "Wrong Parameters. Try again"),
	wxMessageDialog:showModal (D). 

changeIMG(Canvas) ->
	X= random:uniform(10),
	case X of 
		1-> IMG=wxImage:new("/home/barlesh/Erworkspace/FinalProject/GOT_SIM/Files/GOT1.png");
		2-> IMG=wxImage:new("/home/barlesh/Erworkspace/FinalProject/GOT_SIM/Files/GOT2.png");
		3-> IMG=wxImage:new("/home/barlesh/Erworkspace/FinalProject/GOT_SIM/Files/GOT3.png");
		4-> IMG=wxImage:new("/home/barlesh/Erworkspace/FinalProject/GOT_SIM/Files/GOT4.png");
		5-> IMG=wxImage:new("/home/barlesh/Erworkspace/FinalProject/GOT_SIM/Files/GOT5.png");
		6-> IMG=wxImage:new("/home/barlesh/Erworkspace/FinalProject/GOT_SIM/Files/GOT6.png");
		7-> IMG=wxImage:new("/home/barlesh/Erworkspace/FinalProject/GOT_SIM/Files/GOT7.png");
		8-> IMG=wxImage:new("/home/barlesh/Erworkspace/FinalProject/GOT_SIM/Files/GOT8.png");
		9-> IMG=wxImage:new("/home/barlesh/Erworkspace/FinalProject/GOT_SIM/Files/GOT9.png");
		10->IMG=wxImage:new("/home/barlesh/Erworkspace/FinalProject/GOT_SIM/Files/GOT10.png") 
	end,
	ClientDC = wxClientDC:new(Canvas),
	Bitmap = wxBitmap:new(IMG),
	wxDC:drawBitmap(ClientDC, Bitmap, {0,0}),
	wxBitmap:destroy(Bitmap),
	wxClientDC:destroy(ClientDC).
	
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


