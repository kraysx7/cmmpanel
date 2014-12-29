-module(main_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(main_sup, []).

init(_Args) ->
    {ok, {{one_for_one, 100, 1},
	  [
	   %% запускаем драйвер работы с экраном
	   {cecho_srv, {cecho_srv, start_link, []}, permanent, brutal_kill, worker, []},	   
	   %% запускаем контроллер пользовательского интерфейса (зависит от cecho_srv)
	   {gui_cntl, {gui_cntl, start_link, []}, permanent, brutal_kill, worker, []},
	   %% запускаем контроллер ввода от пользователя
	   {tty_cntl, {tty_cntl, start_link, []}, permanent, brutal_kill, worker, []},
	   %% запускаем gen_server для работы с CMM контроллером
	   {cmmcontroller, {cmmcontroller, start_link, []}, permanent, brutal_kill, worker, []}
	  ]}}.
