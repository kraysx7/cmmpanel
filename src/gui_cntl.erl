-module(gui_cntl).

-behaviour(gen_server).

%% API
-export([start_link/0, set_active_node/1, get_active_node/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(UPDATE_INTERVAL, 400). % 1 second
-define(REDRAW_INTERVAL, 50000). % 3 second

-define(TTY_GREEN,  1).
-define(TTY_RED,    2).
-define(TTY_YELLOW, 3).
-define(TTY_CYAN,   4).
-define(TTY_WHITE,  8).

-record(state, {active_node_id}).

%% offsets
-define(OFS_NODES_TABLE_X, 11).
-define(OFS_NODES_TABLE_Y, 4).
-define(NODE_CELL_WIDTH,  8).
-define(NODE_CELL_HEIGHT, 21).

-define(OFS_NODE_ID, 1).
-define(OFS_NODE_I2C_POWER, 2).
-define(OFS_NODE_I2C_COMLINE, 3).
-define(OFS_NODE_USB_FACE_IN, 4).
-define(OFS_NODE_I2C_WD, 5).
-define(OFS_NODE_AMPERE, 6).
-define(OFS_NODE_TEMPERATURE, 11).

-define(OFS_NODE_SENSORS, 7).

-define(OFS_NODE_TESTS, 11).
%%%===================================================================
%%% API
%%%===================================================================

get_active_node_()->
    [{active_node_id, NodeId}] = ets:lookup(state, active_node_id), NodeId.
    %gen_server:call(?MODULE, {get_active_node}).

get_active_node()->
    gen_server:call(?MODULE, {get_active_node}).

set_active_node(NodeId) ->
    gen_server:cast(?MODULE, {set_active_node, NodeId}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(state, [set, named_table]),
    ets:insert(state, {active_node_id, 0}),

    %% Start first full redraw timer
    erlang:send_after(1000, self(), redraw_trigger),
    %% Start update timer
    erlang:send_after(?UPDATE_INTERVAL, self(), update_trigger),
    {ok, #state{active_node_id=0}}.

handle_call({get_active_node}, _From, State) ->
    [{active_node_id, NodeId}] = ets:lookup(state, active_node_id),
    {reply, NodeId, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_active_node, NodeId}, State) ->
    ets:insert(state, {active_node_id, NodeId}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(redraw_trigger, State) ->
    redraw_screen(),
    %% Start new timer
    erlang:send_after(?REDRAW_INTERVAL, self(), redraw_trigger),
    {noreply, State};


handle_info(update_trigger, State) ->
    update_screen(),
    %% Start new timer
    erlang:send_after(?UPDATE_INTERVAL, self(), update_trigger),
    {noreply, State};


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

redraw_screen() ->
    I2CInfo = cmmcontroller:get_i2c_info(),
    tty_cntl:scrclr(),
    %% рисуем состояние i2c
    case I2CInfo of
	{ok, NodesInfo} ->
	    draw_cmm_conn_state(ok),
	    draw_table_legend(round((length(NodesInfo) - 2) / 2)),
	    draw_nodes_table(NodesInfo),
	    draw_i2c_state(NodesInfo);
	{error, nodedown} ->
	    draw_cmm_conn_state(nodedown);
	{error, nodecrashed} -> 
	    draw_cmm_conn_state(nodecrashed)
    end,
    tty_cntl:print_string(1, 1, cyan, "Rikor electronics. cmm panel. v1.0"),
    tty_cntl:print_string(0,0, white, "").

update_screen() ->
    %% рисуем состояние i2c
    case cmmcontroller:get_i2c_info() of
	{ok, NodesInfo} -> 
	    draw_cmm_conn_state(ok),
	    draw_table_legend(round((length(NodesInfo) - 2) / 2)),
	    draw_nodes_table(NodesInfo),
	    draw_i2c_state(NodesInfo);
	{error, nodedown} ->
	    draw_cmm_conn_state(nodedown);
	{error, nodecrashed} ->
	    draw_cmm_conn_state(nodecrashed)
    end,
    %% рисуем значения тестов
    case cmmcontroller:get_tests() of
	{ok, Tests} ->
	    draw_cmm_conn_state(ok),
	    draw_tests(Tests);
	{error, nodedown} ->
	    draw_cmm_conn_state(nodedown);
	{error, nodecrashed} ->
	    draw_cmm_conn_state(nodecrashed)
    end,
    tty_cntl:print_string(1, 1, cyan, "Rikor electronics. cmm panel. v1.0"),
    tty_cntl:print_string(0,0, white, "").


draw_cmm_conn_state(CmmConnState) ->
    case CmmConnState of
	nodedown -> tty_cntl:print_string(45, 1, yellow,    "Нет соединения с CMM ! ");
	nodecrashed -> tty_cntl:print_string(45, 1, yellow, "Ошибка CMM контроллера!");
	_ ->        tty_cntl:print_string(45, 1, yellow,    "                       ")
    end.


draw_table_legend(SlotsCount) ->
    io:format("Slots count: ~p~n", [SlotsCount]),
    %% рисуем имена сенсоров
    Sensors = config:get(sensors),
    draw_sensors_desc(Sensors, length(Sensors)),
    %% рисуем имена тестов
    draw_tests_desc(config:get(tests_desc)),
    {OX1, OY1} = get_node_cell_pos(1),
    {OX2, OY2} = get_node_cell_pos(2),
    %% рисуем номера слотов
    tty_cntl:print_string(OX2 - 8, OY2 - 1, white, io_lib:format("~ts", ["   Slot"])),
    draw_slot_nums(OX2, OY2 - 1, SlotsCount),
    %% рисуем имена i2c параметров
    tty_cntl:print_string(OX1 - ?OFS_NODES_TABLE_X, OY1 + ?OFS_NODE_ID,          white, io_lib:format("~ts", ["     Node"])),
    tty_cntl:print_string(OX1 - ?OFS_NODES_TABLE_X, OY1 + ?OFS_NODE_I2C_POWER,   white, io_lib:format("~ts", ["    Power"])),
    tty_cntl:print_string(OX1 - ?OFS_NODES_TABLE_X, OY1 + ?OFS_NODE_I2C_COMLINE, white, io_lib:format("~ts", ["  Comline"])),
    tty_cntl:print_string(OX1 - ?OFS_NODES_TABLE_X, OY1 + ?OFS_NODE_USB_FACE_IN, white, io_lib:format("~ts", ["  USB/Com"])),
    tty_cntl:print_string(OX1 - ?OFS_NODES_TABLE_X, OY1 + ?OFS_NODE_I2C_WD,      white, io_lib:format("~ts", ["       WD"])),

    tty_cntl:print_string(OX2 - ?OFS_NODES_TABLE_X, OY2 + ?OFS_NODE_ID,          white, io_lib:format("~ts", ["     Node"])),
    tty_cntl:print_string(OX2 - ?OFS_NODES_TABLE_X, OY2 + ?OFS_NODE_I2C_POWER,   white, io_lib:format("~ts", ["    Power"])),
    tty_cntl:print_string(OX2 - ?OFS_NODES_TABLE_X, OY2 + ?OFS_NODE_I2C_COMLINE, white, io_lib:format("~ts", ["  Comline"])),
    tty_cntl:print_string(OX2 - ?OFS_NODES_TABLE_X, OY2 + ?OFS_NODE_USB_FACE_IN, white, io_lib:format("~ts", ["  USB/Com"])),
    tty_cntl:print_string(OX2 - ?OFS_NODES_TABLE_X, OY2 + ?OFS_NODE_I2C_WD,      white, io_lib:format("~ts", ["       WD"])),
    %% рисуем пояснения для ампер и температуры
    %%tty_cntl:print_string(OX1 - ?OFS_NODES_TABLE_X, OY1 + ?OFS_NODE_AMPERE,      white, io_lib:format("~ts", ["   Ampere"])),
    %%tty_cntl:print_string(OX1 - ?OFS_NODES_TABLE_X, OY1 + ?OFS_NODE_TEMPERATURE, white, io_lib:format("~ts", ["     T \xb0C"])),

    %%tty_cntl:print_string(OX2 - ?OFS_NODES_TABLE_X, OY2 + ?OFS_NODE_AMPERE,      white, io_lib:format("~ts", ["   Ampere"])),
    %%tty_cntl:print_string(OX2 - ?OFS_NODES_TABLE_X, OY2 + ?OFS_NODE_TEMPERATURE, white, io_lib:format("~ts", ["     T \xb0C"])),
    %% рисуем разделитель между значчениями i2c и тестами
    %% draw_tests_delimiter(SlotsCount),
    ok.

draw_tests_delimiter(SlotsCount) ->
    {OX1, OY1} = get_node_cell_pos(1),
    {OX2, OY2} = get_node_cell_pos(2),
    draw_hline(OX1, OY1 + ?OFS_NODE_TESTS, ?NODE_CELL_WIDTH * SlotsCount),
    draw_hline(OX2, OY2 + ?OFS_NODE_TESTS, ?NODE_CELL_WIDTH * SlotsCount).


draw_slot_nums(_, _, SlotNum) when SlotNum =< 0 -> ok;
draw_slot_nums(OX, OY, SlotNum) ->
    ResOX = ?NODE_CELL_WIDTH * (SlotNum - 1) + ?OFS_NODES_TABLE_X + 2,
    tty_cntl:print_string(ResOX, OY, magenta, io_lib:format("~p", [SlotNum])),
    draw_slot_nums(OX, OY, SlotNum - 1).


draw_nodes_table([]) -> ok;
draw_nodes_table([NodeI2CState | Other]) ->
    {_,NodeInfo} = NodeI2CState,
    draw_node_cell(NodeInfo),
    %%NodeDescStr = lists:flatten(io_lib:format("Node N~p   t1=24C; t2=45C; boot_type=pxe", [NodesCount])),
    draw_nodes_table(Other).


draw_node_cell([]) -> ok;
draw_node_cell(NodeInfo) ->
    ActiveNodeId = get_active_node_(),
    NodeId = proplists:get_value(node_id, NodeInfo),
    
    {OX, OY} = get_node_cell_pos(NodeId),
    
    %%tty_cntl:print_string(1, 2, white, io_lib:format("active_node: `~w:~w`       ~n", [NodeId, ActiveNodeId])),
    %% отрисовка базовой таблицы
    case NodeId of
	%%27 -> draw_vline(OX+?NODE_CELL_WIDTH, OY, ?NODE_CELL_HEIGHT);
	%%28 -> draw_vline(OX+?NODE_CELL_WIDTH, OY, ?NODE_CELL_HEIGHT);
	0 -> %% CMM CELL
	    draw_vline(OX, OY, white, ?NODE_CELL_HEIGHT*2),
	    draw_vline(OX+?NODE_CELL_WIDTH, OY, white,?NODE_CELL_HEIGHT*2),
	    draw_hline(OX, OY, white, ?NODE_CELL_WIDTH+1);
	ActiveNodeId -> 
	    draw_vline(OX, OY+1, magenta, ?NODE_CELL_HEIGHT-1),
	    draw_hline(OX, OY, magenta, ?NODE_CELL_WIDTH);
	NodeId when NodeId > 0 ->
	    case NodeId - ActiveNodeId of
		2 when ActiveNodeId > 0 -> 
		    %% значит соседняя слева активная, рисуем границу другим цветом
		    draw_vline(OX, OY+1, magenta, ?NODE_CELL_HEIGHT-1),
		    draw_hline(OX, OY, magenta, 1),
		    draw_hline(OX+1, OY, white, ?NODE_CELL_WIDTH-1);
		_ ->
		    draw_vline(OX, OY+1, white, ?NODE_CELL_HEIGHT-1),
		    draw_hline(OX, OY, white, ?NODE_CELL_WIDTH)
	    end;
	_ -> ok
    end.

draw_vline(X, Y, Height) -> draw_vline(X, Y, white, 0, Height).
draw_vline(X, Y, Color, Height) ->  draw_vline(X, Y, Color, 0, Height).
draw_vline(_X, _Y, _Color, Height, Height) -> ok;
draw_vline(X, Y, Color, C, Height) -> 
    tty_cntl:print_string(X, Y+C, Color, "|"),
    draw_vline(X, Y, Color, C+1, Height).


draw_hline(X, Y, Width) -> draw_hline(X, Y, white, 0, Width).
draw_hline(X, Y, Color, Width) -> draw_hline(X, Y, Color, 0, Width).
draw_hline(_X, _Y, _Color, Width, Width) -> ok;
draw_hline(X, Y, Color, C, Width) ->
    tty_cntl:print_string(X+C, Y, Color, "_"),
    draw_hline(X, Y, Color, C+1, Width).

draw_i2c_state([]) -> ok;
draw_i2c_state([NodeInfo | _Other]) when length(NodeInfo) == 0 -> ok;
draw_i2c_state([NodeI2CState | Other]) ->
    {_,NodeInfo} = NodeI2CState,
    NodeId = proplists:get_value(node_id, NodeInfo),
    NodeStatus = proplists:get_value(node_status, NodeInfo),
    Power = proplists:get_value(power, NodeInfo),
    ComLine = proplists:get_value(com_line, NodeInfo),
    UsbFaseIn = proplists:get_value(usb_fase_in, NodeInfo),
    Wd = proplists:get_value(wdt_state, NodeInfo),
    Ampere = proplists:get_value(ampere, NodeInfo),
    _Temperature = proplists:get_value(temperature, NodeInfo),
    {OX, OY} = get_node_cell_pos(NodeId),
    case NodeId of
	NodeId when NodeId > 0 ->
	    %% рисуем номер узла :
	    draw_node_id(OX+1, OY+?OFS_NODE_ID, NodeId, NodeStatus),
	    case NodeStatus of
		enable ->
		    %% рисуем состояние питания
		    draw_power(OX+1, OY+?OFS_NODE_I2C_POWER, Power),
		    %% отображаем знак подключения COM порта и USB порта
		    draw_comline(OX+1, OY+?OFS_NODE_I2C_COMLINE, ComLine),
		    draw_usb_fase_in(OX+1, OY+?OFS_NODE_USB_FACE_IN, UsbFaseIn),
		    %% отображаем состояние Watch Dogа
		    draw_wd(OX+1, OY+?OFS_NODE_I2C_WD, Wd),
		    %% отображаем вольтаж и амперы
		    case Ampere of
			Ampere when is_float(Ampere) -> tty_cntl:print_string(OX+1, OY+?OFS_NODE_AMPERE, white, io_lib:format("~p A", [Ampere]));
			_ -> tty_cntl:print_string(OX+1, OY+?OFS_NODE_AMPERE, white, io_lib:format("~ts",["     "]))
		    end,
		    draw_sensors(NodeInfo);
		%% tty_cntl:print_string(OX+1, OY+?OFS_NODE_TEMPERATURE, white, io_lib:format("~p", [Temperature]));	    
		_ -> ok
	    end,
	    case Power of
		off -> clear_tests(NodeId);
		_ -> ok
	    end;
	0 -> 
	    %% CMM information
	    draw_power(OX+1, OY+?OFS_NODE_I2C_POWER, Power),
	    %% отображаем состояние Watch Dogа
	    draw_wd(OX+1, OY+?OFS_NODE_I2C_WD, Wd),
	    draw_sensors(NodeInfo),
	    draw_cmm_info(NodeInfo);
	_ -> ok
    end,
    draw_i2c_state(Other).


draw_cmm_info(NodeInfo) ->
    NodeId = proplists:get_value(node_id, NodeInfo),
    Ampere = proplists:get_value(ampere, NodeInfo),
    {OX, OY} = get_node_cell_pos(NodeId),
    tty_cntl:print_string(OX+1, OY+?OFS_NODE_ID, white, io_lib:format("-CMM-", [])),
    case Ampere of
	Ampere when is_float(Ampere) -> tty_cntl:print_string(OX+1, OY+?OFS_NODE_AMPERE, white, io_lib:format("~p A", [Ampere]));
	_ -> tty_cntl:print_string(OX+1, OY+?OFS_NODE_AMPERE, white, io_lib:format("~ts",["     "]))
    end.
    
draw_node_id(X, Y, NodeId, NodeStatus) ->
    case NodeStatus of
	enable  -> tty_cntl:print_string(X, Y, white, io_lib:format("~p", [NodeId]));
	disable -> tty_cntl:print_string(X, Y, gray,  io_lib:format("~p", [NodeId]));
	_ -> 	   tty_cntl:print_string(X, Y, red,   io_lib:format("~p", [err]))
    end.
    

draw_power(X, Y, Power) ->
    Format = lists:flatten(io_lib:format("~ts~p~ts",["~-",?NODE_CELL_WIDTH-1,"ts"])),
    case Power of
	on ->  tty_cntl:print_string(X, Y, green, io_lib:format(Format, [Power]));
	off -> tty_cntl:print_string(X, Y, red,   io_lib:format(Format, [Power]));
	_ ->   tty_cntl:print_string(X, Y, red,   io_lib:format(Format, [err]))
    end.

	    
draw_comline(X, Y, ComLine) ->
    case ComLine of
	on ->  tty_cntl:print_string(X, Y, white, io_lib:format("COM",[]));
	off -> tty_cntl:print_string(X, Y, white, io_lib:format("   ",[]));
	_ ->   tty_cntl:print_string(X, Y, red,   io_lib:format("~p", [err]))
    end.

draw_usb_fase_in(X, Y, UsbFaseIn) ->
    case UsbFaseIn of
	on ->  tty_cntl:print_string(X, Y, white,  io_lib:format("IN",[]));
	off -> tty_cntl:print_string(X, Y, white,  io_lib:format("  ",[]));
	_ ->   tty_cntl:print_string(X, Y, red,    io_lib:format("~p", [undef]))
    end. 

  
draw_wd(X, Y, Wd) ->
    case Wd of
	on -> tty_cntl:print_string(X, Y, yellow, io_lib:format("WD    ",[]));
	_ ->  tty_cntl:print_string(X, Y, white,  io_lib:format("      ",[]))
	%Any -> tty_cntl:print_string(X, Y, red, io_lib:format("~p", [Any]))
    end.


draw_sensors(NodeInfo) ->
    %% получаем список сенсоров
    Sensors = config:get(sensors),
    draw_sensors_(Sensors, length(Sensors), NodeInfo).

draw_sensors_([], _, _) -> ok;
draw_sensors_([Sensor | Other], SensorsCount, NodeInfo) ->
    {SysName, _Desc, _EtalonVal, MinVal, MaxVal} = Sensor,
    NodeId = proplists:get_value(node_id, NodeInfo),
    CurVal = proplists:get_value(SysName, NodeInfo),
    {OX, OY} = get_node_cell_pos(NodeId),
    ResOY = OY + ?OFS_NODE_SENSORS + (SensorsCount - length(Other) - 1),
    case CurVal of
	CurVal when CurVal < MinVal ; CurVal > MaxVal ->
	    tty_cntl:print_string(OX+1, ResOY, red,   io_lib:format("~.2f", [CurVal]));
	CurVal when is_float(CurVal) -> 
	    tty_cntl:print_string(OX+1, ResOY, green, io_lib:format("~.2f", [CurVal]));
	CurVal -> 
	    tty_cntl:print_string(OX+1, ResOY, white, io_lib:format("~.2f", [CurVal]))
    end,
    draw_sensors_(Other, SensorsCount, NodeInfo).

draw_sensors_desc([], _) -> ok;
draw_sensors_desc([Sensor | Other], SensorsCount) ->
    {_, Desc, _, _, _} = Sensor,
    {OX1, OY1} = get_node_cell_pos(1),
    {OX2, OY2} = get_node_cell_pos(2),
    ResOX1 = OX1 - 9,
    ResOY1 = OY1 + ?OFS_NODE_SENSORS + (SensorsCount - length(Other) - 1),
    ResOX2 = OX2 - 9,
    ResOY2 = OY2 + ?OFS_NODE_SENSORS + (SensorsCount - length(Other) - 1),
    tty_cntl:print_string(ResOX1, ResOY1, white, io_lib:format("~ts", [Desc])),
    tty_cntl:print_string(ResOX2, ResOY2, white, io_lib:format("~ts", [Desc])),
    draw_sensors_desc(Other, SensorsCount).


clear_tests(NodeId) ->
    clear_tests_(NodeId, config:get(tests_desc)).

clear_tests_(_, []) -> ok;
clear_tests_(NodeId, [{TestId, _} | Other]) ->
    {OX, OY} = get_node_cell_pos(NodeId),
    ResOX = OX + 1,
    ResOY = OY + ?OFS_NODE_TESTS + TestId,
    Format = lists:flatten(io_lib:format("~ts~p~ts",["~-",?NODE_CELL_WIDTH-1,"ts"])),
    tty_cntl:print_string(ResOX, ResOY, white, io_lib:format(Format, [""])),
    clear_tests_(NodeId, Other).
 

draw_tests([]) -> ok;
draw_tests([Test | Other]) ->
    {{NodeId, TestId}, {TestStatus, TestValue}} = Test,
    {OX, OY} = get_node_cell_pos(NodeId),
    ResOX = OX + 1,
    ResOY = OY + ?OFS_NODE_TESTS + TestId,
    Format = lists:flatten(io_lib:format("~ts~p~ts",["~-",?NODE_CELL_WIDTH-1,"ts"])),
    case TestStatus of
	0 -> tty_cntl:print_string(ResOX, ResOY, green_inv,  io_lib:format(Format, [TestValue]));
	1 -> tty_cntl:print_string(ResOX, ResOY, red_inv,    io_lib:format(Format, [TestValue]));
	2 -> tty_cntl:print_string(ResOX, ResOY, yellow,     io_lib:format(Format, [TestValue]));
	99 -> tty_cntl:print_string(ResOX, ResOY, white,     io_lib:format(Format, [TestValue]))
    end,
    draw_tests(Other).


draw_tests_desc([]) -> ok;
draw_tests_desc([{TestId,TestDesc} | Other]) ->
    {OX1, OY1} = get_node_cell_pos(1),
    {OX2, OY2} = get_node_cell_pos(2),
    ResOX1 = OX1 - ?OFS_NODES_TABLE_X,
    ResOY1 = OY1 + ?OFS_NODE_TESTS + TestId,
    ResOX2 = OX2 - ?OFS_NODES_TABLE_X,
    ResOY2 = OY2 + ?OFS_NODE_TESTS + TestId,
    tty_cntl:print_string(ResOX1, ResOY1, white, io_lib:format("~ts", [TestDesc])),
    tty_cntl:print_string(ResOX2, ResOY2, white, io_lib:format("~ts", [TestDesc])),
    draw_tests_desc(Other).



get_node_cell_pos(NodeId) ->
    case NodeId of
	NodeId when NodeId > 0 ->
	    %% получить остаток от деления на 2
	    Rem = NodeId rem 2,
	    %% получить позицию ячейки по оси X
	    OX = round(?NODE_CELL_WIDTH * ((NodeId + Rem) / 2 - 1) + ?OFS_NODES_TABLE_X),
	    %% получить позицию ячейки по оси Y
	    OY = round(?NODE_CELL_HEIGHT * Rem + ?OFS_NODES_TABLE_Y),
	    {OX, OY};
	0 -> %% CMM CELL
	    CmmNodeId = 30,
	    %% получить остаток от деления на 2
	    Rem = CmmNodeId rem 2,
	    %% получить позицию ячейки по оси X
	    OX = round(?NODE_CELL_WIDTH * ((CmmNodeId + Rem) / 2 - 1) + ?OFS_NODES_TABLE_X),
	    %% получить позицию ячейки по оси Y
	    OY = round(?NODE_CELL_HEIGHT * Rem + ?OFS_NODES_TABLE_Y),
	    {OX, OY};
	_ -> ok
    end.
    


    
    
    
    
