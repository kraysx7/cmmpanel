-module(cmmcontroller).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([get_i2c_info/0, get_i2c_info/1, get_tests/0, get_tests/1, clear_tests/1, power/1, bootmode/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% Функция делает rpc запрос к узлу, где работает приложение cmmcontroller.
%% Запрос прозрачен, данные передаются в формате внутренних структур erlang.
%% Результаты запроса :
%% 
get_i2c_info() ->
    gen_server:call(?MODULE, {get_i2c_info}).

get_i2c_info(NodeId) ->
    gen_server:call(?MODULE, {get_i2c_info, NodeId}).

get_tests() ->
    gen_server:call(?MODULE, {get_tests}).

get_tests(NodeId) ->
    gen_server:call(?MODULE, {get_tests, NodeId}).

clear_tests(NodeId) ->
    gen_server:call(?MODULE, {clear_tests, NodeId}).

power(0) -> bad_node_id;
power(NodeId) ->
    gen_server:call(?MODULE, {power, NodeId}).

bootmode(NodeId) ->
    bootmode(NodeId, pxe).

bootmode(NodeId, pxe) ->
    gen_server:cast(?MODULE, {bootmode, NodeId, pxe});

bootmode(NodeId, hdd) ->
    gen_server:cast(?MODULE, {bootmode, NodeId, hdd}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.


handle_call({get_i2c_info}, _From, State) ->
    Node = config:get(cmmcontroller_node),
    Mod = 'nodes_state',
    Fun = 'get_i2c_state',
    Args = [],
    Result = rpc:call(Node, Mod, Fun, Args),
    %%io:format("Result: ~p~n",[Result]),
    case Result of
	{ok, I2CInfo} -> {reply, {ok, I2CInfo}, State};
	{badrpc, nodedown} ->  {reply, {error, nodedown}, State};		  
	{badrpc, {'EXIT', _Reason}} ->  {reply, {error, nodecrashed}, State}
    end;


handle_call({get_i2c_info, NodeNum}, _From, State) ->
    Node = config:get(cmmcontroller_node),
    Mod = 'nodes_state',
    Fun = 'get_i2c_state',
    Args = [NodeNum],
    Result = rpc:call(Node, Mod, Fun, Args),
    case Result of
	{ok, I2CInfo} -> {reply, {ok, I2CInfo}, State};
	{badrpc, nodedown} ->  {reply, {error, nodedown}, State};		  
	{badrpc, {'EXIT', _Reason}} ->  {reply, {error, nodecrashed}, State}
    end;

handle_call({get_tests}, _From, State) ->
    Node = config:get(cmmcontroller_node),
    Mod = 'rpc_tests_handler',
    Fun = 'get_tests',
    Args = [],
    Result = rpc:call(Node, Mod, Fun, Args),
    case Result of
	{ok, Tests} -> {reply, {ok, Tests}, State};
	{badrpc, nodedown} ->  {reply, {error, nodedown}, State};		  
	{badrpc, {'EXIT', _Reason}} ->  {reply, {error, nodecrashed}, State}
    end;

handle_call({get_tests, NodeId}, _From, State) ->
    Node = config:get(cmmcontroller_node),
    Mod = 'rpc_tests_handler',
    Fun = 'get_tests',
    Args = [NodeId],
    Result = rpc:call(Node, Mod, Fun, Args),
    case Result of
	{ok, Tests} -> {reply, {ok, Tests}, State};
	{badrpc, nodedown} ->  {reply, {error, nodedown}, State};		  
	{badrpc, {'EXIT', _Reason}} ->  {reply, {error, nodecrashed}, State}
    end;

handle_call({clear_tests, NodeId}, _From, State) ->
    Node = config:get(cmmcontroller_node),
    Mod = 'rpc_tests_handler',
    Fun = 'clear_tests',
    Args = [NodeId],
    Result = rpc:call(Node, Mod, Fun, Args),
    case Result of
	{ok, Tests} -> {reply, {ok, Tests}, State};
	{badrpc, nodedown} ->  {reply, {error, nodedown}, State};		  
	{badrpc, {'EXIT', _Reason}} ->  {reply, {error, nodecrashed}, State}
    end;


handle_call({power, NodeId}, _From, State) ->
    Node = config:get(cmmcontroller_node),
    Mod = 'rpc_i2c_handler',
    Fun = 'power',
    Args = [NodeId],
    Result = rpc:call(Node, Mod, Fun, Args),
    case Result of
	{ok, NodeId} -> {reply, {ok, NodeId}, State};
	{badrpc, nodedown} ->  {reply, {error, nodedown}, State};		  
	{badrpc, {'EXIT', _Reason}} ->  {reply, {error, nodecrashed}, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({bootmode, NodeId, Mode}, State) ->
    Node = config:get(cmmcontroller_node),
    Mod = 'nodes_state',
    Fun = 'bootmode',
    Args = [NodeId, Mode],
    rpc:cast(Node, Mod, Fun, Args),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
