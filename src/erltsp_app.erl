-module(erltsp_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	handle_start_erltsp_sup(erltsp_sup:start_link()).

handle_start_erltsp_sup(Error = {error, _}) ->
    Error;
handle_start_erltsp_sup(Msg) ->
    ok = handle_start_console_logger(),
    Msg.

handle_start_console_logger() ->
    ok = erltsp_event_console:add_handler(),
    ok.

stop(_State) ->
	ok.
