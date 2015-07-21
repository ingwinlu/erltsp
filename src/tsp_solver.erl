-module(tsp_solver).

-callback init(Problem :: tsp_problem:tsp_problem()) ->
    {ok, Runner :: pid()}.

-callback run() -> ok.
