-module(erltsp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
        erltsp_event(),
        erltsp_solver_sup()
    ],
    {ok, {sup_flags(), Procs}}.


sup_flags() ->
    #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    }.

erltsp_event() ->
    #{
        id => erltsp_event,
        start => {
                    erltsp_event,
                    start_link,
                    []
                 },
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [erltsp_event]
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
