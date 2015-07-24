-module(erltsp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
        tsp_event(),
        erltsp_solver_sup()
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

erltsp_solver_sup() ->
    #{
        id => erltsp_solver_sup,
        start => {
                    erltsp_solver_sup,
                    start_link,
                    []
        },
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [erltsp_solver_sup]

    }.
