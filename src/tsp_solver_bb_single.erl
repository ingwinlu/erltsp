-module(tsp_solver_bb_single).

-export([init/1, iterate/1, best/1]).

-behaviour(tsp_solver).

-record(state, {
          problem,
          queue,
          upper_bound,
          best
}).

-record(bb_problem, {
          fixed,
          allowed,
          lower_bound
}).

-spec init(Problem :: tsp_problem:tsp_problem()) -> {ok, State0 :: term()}.
init(Problem) ->
    Dimension = tsp_problem:dimension(Problem),
    InitialBBProblem = initial_bb_problem(Dimension),
    Queue = queue:new(),
    Queue1 = queue:in(InitialBBProblem, Queue),
    State0 = #state{
        problem=Problem,
        queue=Queue1
    },
    {ok, State0}.

-spec iterate(State0 :: term()) -> {ok, State1 :: term()}.
iterate(State = #state{}) ->
    % check if lower bound is bigger than upper_bound,
    % skip if true
    %
    % if allowed = [], finish and look if
    % new best solution was found
    %
    % estimate lower_bound for branch
    %
    % else, spawn new problems with a new node picked
    % set their new lower bound
    % add problems to queue that qualify
    {ok, State}.

-spec best(State :: term()) -> {ok, Best :: term()}.
best(#state{upper_bound=UpperBound, best=Best}) ->
    {ok, {UpperBound, Best}}.

initial_bb_problem(Dimension) ->
    #bb_problem{
       fixed = [],
       allowed = lists:seq(1,Dimension),
       lower_bound = 0
    }.

h1(LowerBound, AllowedNodes, Edges) ->
    LowerBound.
