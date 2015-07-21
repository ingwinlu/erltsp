-module(tsp_solver).

-callback init(Problem :: tsp_problem:tsp_problem()) -> {ok, State0 :: term()}.
-callback iterate(State0 :: term()) -> {ok, State1 :: term()}.
-callback best(State :: term()) -> {ok, Best :: term()}.
