-module(main_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    application:start(sasl), %% приложение для вывода отладочной инф.
    main_sup:start_link().

stop(_State) ->
    ok.
