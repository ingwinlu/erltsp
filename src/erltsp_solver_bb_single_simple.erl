-module(erltsp_solver_bb_single_simple).

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
          fixed,
          edge_dict,
          from_nodes,
          to_nodes,
          lower_bound
}).

% API
-spec start_link(Problem :: erltsp_problem:problem()) ->
    {ok, Pid :: pid()}.
start_link(Problem) ->
    erltsp_solver:start_link(Problem, ?MODULE).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    erltsp_solver:stop(Pid).


% init
-spec init(Problem :: erltsp_problem:problem()) -> {ok, State0 :: term()}.
init(Problem) ->
    Dimension = erltsp_problem:dimension(Problem),
    EdgeDict = erltsp_problem:edgedict(Problem),
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
       fixed = [],
       edge_dict = EdgeDict,
       from_nodes = AllNodes,
       to_nodes = AllNodes,
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
        abort -> do_return(State1);
        continue -> calculate_lower_bound(Problem, State1)
    end.

do_validate_problem(#bb_problem{lower_bound=LowerBound},
                    #state{upper_bound=UpperBound}) ->
    handle_validate_problem(LowerBound, UpperBound).

handle_validate_problem(LowerBound, UpperBound) when 
        is_number(UpperBound), UpperBound < LowerBound ->
    abort;
handle_validate_problem(_LowerBound, _UpperBound) -> 
    continue.

do_return(State) ->
    {ok, State}.

calculate_lower_bound(
        Problem = #bb_problem{lower_bound=LowerBound,
                              edge_dict=EdgeDict,
                              from_nodes=FromNodes,
                              to_nodes=ToNodes},
        State = #state{}) ->
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
        abort -> do_return(State);
        continue -> calculate_upper_bound(Problem1, State)
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
                gb_trees:enter(Distance, Edge, Tree)
        end,
        gb_trees:empty(),
        EdgeKeys
    ),
    {Minimum, _SmallestEdge} = gb_trees:smallest(GbTree),
    EdgeDict1 = lists:foldl(
        fun
            (EdgeKey, Dict) ->
                dict:update(
                    EdgeKey,
                    fun
                        (Edge) ->
                            #{distance:=Distance} = Edge,
                            Edge#{distance:=Distance-Minimum}
                    end,
                    Dict
                )
        end,
        EdgeDict,
        EdgeKeys
    ),
    LowerBound1 = LowerBound + Minimum,
    {ok, EdgeDict1, LowerBound1}.

calculate_upper_bound(BBProblem = #bb_problem{
                                   fixed=Fixed,
                                   edge_dict=EdgeDict,
                                   to_nodes=ToNodes
                                  },
                      State = #state{
                                 problem=Problem,
                                 upper_bound=UpperBound
                                }) ->
    {ok, Solution} = nearestNeighbour(Fixed, ToNodes, EdgeDict),
    {ok, SolutionLength} = erltsp_problem:solution(Problem, Solution),
    case maybe_improvement(SolutionLength, UpperBound) of
        true ->
            State1 = State#state{upper_bound=SolutionLength,
                                 best=Solution},
            %TODO clear state1 queue of 'bad' solutions
            branch(BBProblem, State1);
        false ->
            branch(BBProblem, State)
    end.

nearestNeighbour(Fixed, [], _EdgeDict) ->
    {ok, Fixed};
nearestNeighbour([], ToNodes, EdgeDict) ->
    Keys = dict:fetch_keys(EdgeDict),
    {ok, Edge} = shortest_route(Keys, EdgeDict),
    #{from := From, to := To} = Edge,
    Fixed = [To, From],
    ToNodes1 = ToNodes -- Fixed,
    nearestNeighbour(Fixed, ToNodes1, EdgeDict);
nearestNeighbour([From | _] = Fixed, ToNodes, EdgeDict) ->
    Keys = [
        {From, To} || To <- ToNodes
    ],
    {ok, Edge} = shortest_route(Keys, EdgeDict),
    #{from := From, to := To} = Edge,
    ToNodes1 = ToNodes -- [To],
    Fixed1 = [To | Fixed],
    nearestNeighbour(Fixed1, ToNodes1, EdgeDict).

shortest_route(Keys, EdgeDict) ->
    ShortestEdge = undefined,
    shortest_route(Keys, EdgeDict, ShortestEdge).

shortest_route([], _, Shortest) ->
    return_route(Shortest);
shortest_route([Key | Keys], EdgeDict, ShortestEdge) ->
    Edge = dict:fetch(Key, EdgeDict),
    is_shortest_route(Edge, Keys, EdgeDict, ShortestEdge).

is_shortest_route(#{distance:=0.0} = Edge, _Keys, _EdgeDict, _Shortest) ->
    return_route(Edge);
is_shortest_route(#{distance:=NewDist}, Keys, EdgeDict,
                  #{distance:=Dist} = Shortest) when NewDist > Dist ->
    shortest_route(Keys, EdgeDict, Shortest);
is_shortest_route(Edge, Keys, EdgeDict, _Shortest) ->
    shortest_route(Keys, EdgeDict, Edge).

return_route(Route) ->
    {ok, Route}.

maybe_improvement(NewLength, OldLength)
        when is_number(OldLength), OldLength < NewLength ->
    false;
maybe_improvement(_, _) ->
    true.

branch(Problem, State) ->
    #bb_problem{fixed=Fixed, edge_dict=EdgeDict, from_nodes=FromNodes
               ,to_nodes=ToNodes, lower_bound=LowerBound} = Problem,
    branch(ToNodes, Fixed, EdgeDict, FromNodes, ToNodes, LowerBound, State).

branch([], _, _, _, _, _, State) ->
    do_return(State);
branch([To | Candidates], [], EdgeDict, FromNodes, ToNodes, LowerBound,
       State = #state{queue=Queue}) ->
    RemoveKeys = [
        {From, To} || From <- FromNodes
    ],
    {ok, EdgeDict1} = edgedict_remove_keys(RemoveKeys, EdgeDict),
    Fixed = [To],
%    FromNodes1 = FromNodes -- [To], still need to leave To
    ToNodes1 = ToNodes -- [To],
    BBProblem = #bb_problem{
        fixed = Fixed,
        edge_dict = EdgeDict1,
        from_nodes = FromNodes,
        to_nodes = ToNodes1,
        lower_bound = LowerBound
    },
    Queue1 = queue:in(BBProblem, Queue),
    State1 = State#state{queue=Queue1},
    branch(Candidates, [], EdgeDict, FromNodes, ToNodes, LowerBound, State1);
branch([To | Candidates], [From | _ ] = Fixed, EdgeDict, FromNodes,
       ToNodes, LowerBound,
       State = #state{problem=Problem, queue=Queue}) ->
    EdgeKey = {From, To},
    Edge = dict:fetch(EdgeKey, erltsp_problem:edgedict(Problem)),
    #{distance:=Distance} = Edge,
    RemoveKeysTo = [
        {F, To} || F <- FromNodes
    ],
    {ok, EdgeDict1} =edgedict_remove_keys(RemoveKeysTo, EdgeDict),
    RemoveKeysFrom = [
        {From, T} || T <- ToNodes
    ],
    {ok, EdgeDict2} = edgedict_remove_keys(RemoveKeysFrom, EdgeDict1),

    Fixed1 = [To | Fixed],
    FromNodes1 = FromNodes -- [From],
    ToNodes1 = ToNodes -- [To],
    LowerBound1 = LowerBound + Distance,
    BBProblem = #bb_problem{
        fixed = Fixed1,
        edge_dict = EdgeDict2,
        from_nodes = FromNodes1,
        to_nodes = ToNodes1,
        lower_bound = LowerBound1
    },
    %TODO check lower bound of new problem, don't add when to high
    Queue1 = queue:in(BBProblem, Queue),
    State1 = State#state{queue=Queue1},
    branch(Candidates, Fixed, EdgeDict, FromNodes, ToNodes, LowerBound, State1).

edgedict_remove_keys([], EdgeDict) ->
    {ok, EdgeDict};
edgedict_remove_keys([Key | Keys], EdgeDict) ->
    EdgeDict1 = dict:erase(Key, EdgeDict),
    edgedict_remove_keys(Keys, EdgeDict1).
    

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


stringify_bb(#bb_problem{fixed=Fixed, from_nodes=From, to_nodes=To,
                         lower_bound=Lower}) ->
    lists:flatten(
        io_lib:format("fixed: ~p~n
                       from:  ~p~n
                       to:    ~p~n
                       lower: ~p~n", [Fixed, From, To, Lower])
    ).
