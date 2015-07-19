-module(erltsp_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	case erltsp_sup:start_link() of
        {ok, Pid} ->
            tsp_event_logger:add_handler(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
	ok.
