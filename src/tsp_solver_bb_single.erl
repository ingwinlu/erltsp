-module(tsp_solver_bb_single).

-export([init/1, iterate/1, best/1]).

-behaviour(tsp_solver).

-spec init(Problem :: tsp_problem:tsp_problem()) -> {ok, State0 :: term()}.
init(Problem) -> {ok, state}.

-spec iterate(State0 :: term()) -> {ok, State1 :: term()}.
iterate(State) -> {ok, state1}.

-spec best(State :: term()) -> {ok, Best :: term()}.
best(State) -> {ok, 0}.
