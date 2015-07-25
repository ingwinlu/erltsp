-module(erltsp_solver_bb_single).

-export([start_link/1, stop/1]).
-export([init/1, iterate/1, best/1]).

-behaviour(erltsp_solver).

-record(state, {
          problem,
          queue,
          upper_bound,
          best
}).

-record(bb_problem, {
          added,
          deleted,
          from_nodes,
          to_nodes,
          edge_dict,
          lower_bound
}).

% API
-spec start_link(Problem :: tsp_problem:tsp_problem()) ->
    {ok, Pid :: pid()}.
start_link(Problem) ->
    erltsp_solver:start_link(Problem, ?MODULE).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    erltsp_solver:stop(Pid).


% init
-spec init(Problem :: tsp_problem:tsp_problem()) -> {ok, State0 :: term()}.
init(Problem) ->
    Dimension = tsp_problem:dimension(Problem),
    EdgeDict = tsp_problem:edgedict(Problem),
    InitialBBProblem = initial_bb_problem(Dimension, EdgeDict),
    Queue = queue:in(InitialBBProblem, queue:new()),
    State0 = #state{
        problem=Problem,
        queue=Queue
    },
    {ok, State0}.

initial_bb_problem(Dimension, EdgeDict) ->
    AllNodes = lists:seq(1, Dimension),
    #bb_problem{
       added = [],
       deleted = [],
       from_nodes = AllNodes,
       to_nodes = AllNodes,
       edge_dict = EdgeDict,
       lower_bound = 0
    }.


% iterate
-spec iterate(State0 :: term()) -> {ok, State1 :: term()}.
iterate(State = #state{queue=Queue}) ->
    get_problem(queue:out(Queue), State).

get_problem({empty, _}, State) ->
    {stop, State};
get_problem({{value, Problem}, Queue1}, State) ->
    State1 = State#state{queue=Queue1},
    case do_validate_problem(Problem, State1) of 
        {abort, State2} -> do_return(State2);
        {continue, Problem, State} -> calculate_lower_bound(Problem, State)
    end.

do_validate_problem(Problem = #bb_problem{lower_bound=LowerBound},
                    State = #state{upper_bound=UpperBound}) ->
    handle_validate_problem(LowerBound, UpperBound, Problem, State).

handle_validate_problem(LowerBound, UpperBound, _Problem, State) when 
        is_number(UpperBound), UpperBound > LowerBound ->
    {abort, State};
handle_validate_problem(_LowerBound, _UpperBound, Problem, State) -> 
    {continue, Problem, State}.

do_return(State) ->
    {ok, State}.

calculate_lower_bound(
        Problem = #bb_problem{lower_bound=LowerBound,
                              edge_dict=EdgeDict,
                              from_nodes=FromNodes,
                              to_nodes=ToNodes},
        State) ->
    {ok, EdgeDict1, LowerBound1} = minimize_from(
                                     FromNodes, ToNodes,
                                     EdgeDict, LowerBound
    ),
    {ok, EdgeDict2, LowerBound2} = minimize_to(
                                     FromNodes, ToNodes,
                                     EdgeDict1, LowerBound1
    ),
    Problem1 = Problem#bb_problem{
                 lower_bound=LowerBound2,
                 edge_dict=EdgeDict2},
    case do_validate_problem(Problem1, State) of
        {abort, State1} -> do_return(State1);
        {continue, Problem2, State2} -> calculate_upper_bound(Problem2, State2)
    end.

minimize_from([], _ToNodes, EdgeDict, LowerBound) ->
    {ok, EdgeDict, LowerBound};
minimize_from([From | FromNodes], ToNodes, EdgeDict, LowerBound) ->
    EdgeKeys = keys_from_to([From], ToNodes),
    {ok, EdgeDict1, LowerBound1} = minimize(EdgeKeys, EdgeDict, LowerBound),
    minimize_from(FromNodes, ToNodes, EdgeDict1, LowerBound1).

minimize_to(_FromNodes, [], EdgeDict, LowerBound) ->
    {ok, EdgeDict, LowerBound};
minimize_to(FromNodes, [To | ToNodes], EdgeDict, LowerBound) ->
    EdgeKeys = keys_from_to(FromNodes, [To]),
    {ok, EdgeDict1, LowerBound1} = minimize(EdgeKeys, EdgeDict, LowerBound),
    minimize_to(FromNodes, ToNodes, EdgeDict1, LowerBound1).

keys_from_to(FromList, ToList) ->
    [ {From, To} || From <- FromList, To <- ToList, From /= To ].

minimize(EdgeKeys, EdgeDict, LowerBound) ->
    GbTree = lists:foldl(
        fun
            (EdgeKey, Tree) ->
                Edge = dict:fetch(EdgeKey, EdgeDict),
                #{distance:=Distance} = Edge,
                gb_trees:insert(Distance, Edge, Tree)
        end,
        gb_trees:empty(),
        EdgeKeys
    ),
    {Minimum, _SmallestEdge} = gb_trees:smallest(GbTree),
    EdgeDict1 = lists:foldl(
        fun
            (EdgeKey, Dict) ->
                dict:update(EdgeKey,
                            fun(Old) -> Old - Minimum end,
                            Dict
                )
        end,
        EdgeDict,
        EdgeKeys
    ),
    LowerBound1 = LowerBound + Minimum,
    {ok, EdgeDict1, LowerBound1}.

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

% best
-spec best(State :: term()) -> {ok, Best :: term()}.
best(#state{upper_bound=UpperBound, best=Best}) ->
    {ok, {UpperBound, Best}}.

