-module(erltsp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
        tsp_event(),
        tsp_runner()
    ],
    {ok, {sup_flags(), Procs}}.


sup_flags() ->
    #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    }.

tsp_event() ->
    #{
        id => tsp_event,
        start => {
                    tsp_event,
                    start_link,
                    []
                 },
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [tsp_event]
    }.

tsp_runner() ->
    #{
        id => tsp_runner,
        start => {
                    tsp_runner,
                    start_link,
                    []
                 },
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [tsp_runner]
    }.

