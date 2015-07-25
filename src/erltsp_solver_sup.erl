-module(erltsp_solver_sup).
-behaviour(supervisor).

-export([start_link/0, run/2, stop/1]).
-export([init/1]).

% API
-spec start_link() ->
    {ok, Pid :: pid()} |
    {error, Reason :: term()} |
    ignore.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec run(Problem :: erltsp_problem:problem(), Solver :: atom()) ->
    {ok, Pid ::pid()}.
run(Problem, Solver) ->
    supervisor:start_child(?MODULE, [Problem, Solver]).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    erltsp_solver:stop(Pid).

% Callback
init([]) ->
    Procs = [
             erltsp_solver()
    ],
    {ok, {sup_flags(), Procs}}.

% Helper
sup_flags() ->
    #{
        strategy => simple_one_for_one,
        intensity => 1,
        period => 5
    }.

erltsp_solver() ->
    #{
        id => erltsp_solver,
        start => {
                    erltsp_solver,
                    start_link,
                    []
                 },
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [erltsp_solver]
    }.
