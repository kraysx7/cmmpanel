-module(tty_cntl).

-behaviour(gen_server).

%% API
-export([start_link/0, scrclr/0, print_string/4, add_getch_listener/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(ANSI_COLOR_GRAY    ,"\x1b[1;30m").
-define(ANSI_COLOR_RED     ,"\x1b[0;31;40m").
-define(ANSI_COLOR_GREEN   ,"\x1b[0;32;40m").
-define(ANSI_COLOR_RED_INV   ,"\x1b[0;30;41m").
-define(ANSI_COLOR_GREEN_INV ,"\x1b[0;30;42m").
-define(ANSI_COLOR_YELLOW  ,"\x1b[33m").
-define(ANSI_COLOR_BLUE    ,"\x1b[34m").
-define(ANSI_COLOR_MAGENTA ,"\x1b[1;35m").
-define(ANSI_COLOR_CYAN    ,"\x1b[36m").
-define(ANSI_COLOR_RESET   ,"\x1b[0m").

%% codes for tty driver
-define(TTY_PACKET_SCRCLR,         1).
-define(TTY_PACKET_PRINT_STRING,   2).

-record(state, {tty_printer_port = 0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

scrclr() ->
    gen_server:cast(?SERVER, {scrclr}).

print_string(PosX, PosY, Color, String) ->
    gen_server:call(?SERVER, {print_string, PosX, PosY, Color, String}).

add_getch_listener(GuiCntl) ->
    gen_server:call(?SERVER, {add_getch_listener, GuiCntl}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit,true),
    ets:new(getch_listeners, [set, named_table]),
    spawn(fun() -> getch_thread(null) end),
    {ok, #state{tty_printer_port = null}}.


handle_call({print_string, PosX, PosY, Color, String}, _From, State) ->
    {UTFLenght, BinLenght, BinString} = get_bin_from_string(String),
    io:fwrite("\e[~p;~pH", [PosY, PosX]),
    
    %% set color
    case Color of
	white -> io:put_chars(?ANSI_COLOR_RESET);
	gray ->  io:put_chars(?ANSI_COLOR_GRAY);
	red -> io:put_chars(?ANSI_COLOR_RED);
	green -> io:put_chars(?ANSI_COLOR_GREEN);
	red_inv -> io:put_chars(?ANSI_COLOR_RED_INV);
	green_inv -> io:put_chars(?ANSI_COLOR_GREEN_INV);
	yellow -> io:put_chars(?ANSI_COLOR_YELLOW);
	blue -> io:put_chars(?ANSI_COLOR_BLUE);
	magenta -> io:put_chars(?ANSI_COLOR_MAGENTA);
	cyan -> io:put_chars(?ANSI_COLOR_CYAN);
	_ -> io:put_chars(?ANSI_COLOR_RESET)
    end,

    io:put_chars(String),

    %% reset color
    io:put_chars(?ANSI_COLOR_RESET),
    {reply, ok, State};


handle_call({add_getch_listener, GuiCntl}, _From, State) ->
    ets:insert(getch_listeners, {GuiCntl}),
    {reply, ok, State};

%% ets:match_object(getch_listeners, {'_'}).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({scrclr}, State) ->
    io:fwrite("\e[1;1H\e[2J"),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Процесс ожидания ввода пользователя
getch_thread(State) ->
    Key = cecho_srv:getch(),
    case Key of
	$n -> gui_cntl:set_active_node(0), getch_thread(input_node_id);
	$p -> cmmcontroller:power(gui_cntl:get_active_node());
	$b -> cmmcontroller:bootmode(gui_cntl:get_active_node());
	65 -> gui_cntl:set_active_node(gui_cntl:get_active_node() +1); % key left
	66 -> gui_cntl:set_active_node(gui_cntl:get_active_node() -1); % key down
	67 -> gui_cntl:set_active_node(gui_cntl:get_active_node() +2); % key right
	68 -> gui_cntl:set_active_node(gui_cntl:get_active_node() -2); % key left
	Key when Key>=48,Key=<57 ->
	    case State of
		input_node_id ->
		    ActiveNodeId = gui_cntl:get_active_node(),
		    NewActiveNodeId = list_to_integer(lists:flatten(io_lib:format("~p~p", [ActiveNodeId,Key-48]))),
		    gui_cntl:set_active_node(NewActiveNodeId)
	    end;
	eof -> eof;
	_ -> ok
    end,
    getch_thread(State).


%%%===================================================================
%%% Internal functions
%%%===================================================================


get_bin_from_string(String) when is_list(String)->
    BinString = unicode:characters_to_binary(String, utf8, {utf32, little}),
    BinLenght = byte_size(BinString),
    {BinLenght div 4, BinLenght, BinString};

get_bin_from_string(String) ->
    io:format("get_bin_from_string: ~p~n", [String]),
    get_bin_from_string("ERROR STRING").
