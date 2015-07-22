-module(erltsp).

-export([start/0, stop/0]).
-export([all_solvers/0]).
-export([load_problem/1]).
-export([set_problem/1, set_solver/1, solver_run/0,
         solver_stop/0, solver_best/0]).

-spec start() -> {ok, [StartedApp :: atom()]}.
start() ->
    application:ensure_all_started(erltsp).

-spec stop() -> ok.
stop() ->
    application:stop(erltsp).

-spec all_solvers() -> [Runner :: atom()].
all_solvers() ->
    [tsp_solver_evo_single].

-spec load_problem(File :: file:name_all()) -> Problem :: tsp_problem:tsp_problem().
load_problem(File) -> tsp_problem:from_file(File).

-spec set_problem(Problem :: tsp_problem:tsp_problem()) -> ok.
set_problem(Problem) -> tsp_runner:set_problem(Problem).

-spec set_solver(Solver :: atom()) -> ok.
set_solver(Solver) -> tsp_runner:set_solver(Solver).

-spec solver_run() -> ok | {error, Reason :: atom()}.
solver_run() -> tsp_runner:run().

-spec solver_stop() -> {ok, State :: term()}.
solver_stop() -> tsp_runner:stop().

-spec solver_best() -> {ok, Best :: number()}.
solver_best() -> tsp_runner:best().
