-module(tsp_problem).

-export([from_file/1, solution/2]).
-export([file/1,
         dimension/1,
         threshold/1,
         nodedict/1,
         edgedict/1,
         precedences/1
        ]).
-export_type([tsp_problem/0]).

% TYPES
-type tsp_problem() :: #{
        file => Filename :: file:name_all(),
        dimension => Dimension :: non_neg_integer(),
        threshold => Threshold :: number(),
        nodedict => NodeDict :: tsp_nodedict(),
        edgedict => EdgeDict :: tsp_edgedict(),
        precedences => Precedences :: [precedence()]
       }.

-type tsp_nodedict() :: dict:dict(
                          Id :: non_neg_integer(),
                          Tsp_Node :: tsp_node()).

-type tsp_node() :: #{
        id => Id :: non_neg_integer(),
        x => X_Coordinate :: integer(),
        y => Y_Coordinate :: integer()
       }.

-type tsp_edgedict() :: dict:dict(
        {non_neg_integer(), non_neg_integer()},
        tsp_edge()
    ).

-type tsp_edge() :: #{
        from => non_neg_integer(),
        to => non_neg_integer(),
        distance => float()
       }.

-type precedence() :: {First :: non_neg_integer(),
                       Second :: non_neg_integer()}.

% API
-spec from_file(File :: file:name_all()) -> Problem :: tsp_problem().
from_file(File) ->
    {ok, IODev} = file:open(File, [read, read_ahead]),
    {ok, Dimension} = parse_dimension(IODev),
    {ok, Threshold} = parse_threshold(IODev),
    {ok, "NODE_COORD_SECTION\n"} = file:read_line(IODev),
    {ok, NodeDict} = parse_nodes(IODev, Dimension),
    {ok, EdgeDict} = build_edges(NodeDict),
    {ok, "PRECEDENCE_SECTION\n"} = file:read_line(IODev),
    {ok, PrecedenceList} = parse_precedences(IODev),
    Problem = #{
        file => File,
        dimension => Dimension,
        threshold => Threshold,
        nodedict => NodeDict,
        edgedict => EdgeDict,
        precedences => PrecedenceList
    },
    tsp_event:load_problem(Problem),
    Problem.

-spec solution(Problem :: tsp_problem(), Solution :: [non_neg_integer()]) ->
    {ok, Value :: float()} |
    {error, invalid_solution}.
solution(Problem, Solution) ->
    try validate_solution(Problem, Solution) of
        ok -> grade_solution(Problem, Solution)
    catch
        throw:Error -> {error, Error}
    end.

file(Problem) -> maps:get(file, Problem).
dimension(Problem) -> maps:get(dimension, Problem).
threshold(Problem) -> maps:get(threshold, Problem).
nodedict(Problem) -> maps:get(nodedict, Problem).
edgedict(Problem) -> maps:get(edgedict, Problem).
precedences(Problem) -> maps:get(precedences, Problem).


% PATTERNS
pattern_dimension() -> re:compile("DIMENSION:\\\s+([0-9]+)").
pattern_threshold() -> re:compile("THRESHOLD:\\\s+([0-9]+(\\\.[0-9]+)?)").
pattern_node() -> re:compile("\\\s*([0-9]+)\\s+([0-9]+(\\\.[0-9]+)?)\\s+([0-9]+(\\\.[0-9]+)?)").
pattern_precedence() -> re:compile("\\\s*([0-9]+)\\\s+([0-9]+)").

re_options() -> [
    {capture, all, list}
].


%% PRIV
% from_file
parse_dimension(IODev) ->
    {ok, Line} = file:read_line(IODev),
    parse_pattern_to_int(Line, pattern_dimension()).

parse_threshold(IODev) ->
    {ok, Line} = file:read_line(IODev),
    parse_pattern_to_int(Line, pattern_threshold()).

parse_pattern_to_int(Line, {ok, Pattern}) ->
    {match, [_, Raw | _]} = re:run(Line, Pattern, re_options()),
    {ok, parse_maybe_float(Raw)}.

parse_nodes(IODev, ToParse) ->
    {ok, Pattern} = pattern_node(),
    parse_nodes(IODev, ToParse, Pattern, dict:new()).

parse_nodes(_IODev, 0, _Pattern, Dict) ->
    {ok, Dict};
parse_nodes(IODev, ToParse, Pattern, Dict) ->
    {ok, Line} = file:read_line(IODev),
    {Key, Value} = parse_node(Line, Pattern),
    NewDict = dict:store(Key, Value, Dict),
    parse_nodes(IODev, ToParse-1, Pattern, NewDict).

parse_node(Line, Pattern) ->
    {match, [_, RawId, RawX, _, RawY | _]} = re:run(
        Line,
        Pattern,
        re_options()
    ),
    Id = list_to_integer(RawId),
    X = parse_maybe_float(RawX),
    Y = parse_maybe_float(RawY),
    {Id, #{
        id => Id,
        x => X,
        y => Y
    }}.

build_edges(NodeDict) ->
    NodeKeys = dict:fetch_keys(NodeDict),
    EdgeList = [
        build_edge(NodeDict, From, To)
        || From <- NodeKeys, To <- NodeKeys, From /= To
    ],
    EdgeDict = dict:from_list(EdgeList),
    {ok, EdgeDict}.

build_edge(NodeDict, From, To) ->
    Key = {From, To},
    Value = #{
        from => From,
        to => To,
        distance => distance(
            dict:fetch(From, NodeDict),
            dict:fetch(To, NodeDict)
        )
    },
    {Key, Value}.


distance(Node1, Node2) ->
    X1 = maps:get(x, Node1),
    X2 = maps:get(x, Node2),
    Y1 = maps:get(y, Node1),
    Y2 = maps:get(y, Node2),

    XPow = math:pow(X1-X2,2),
    YPow = math:pow(Y1-Y2,2),
    math:sqrt(XPow + YPow).

parse_precedences(IODev) ->
    {ok, Pattern} = pattern_precedence(),
    parse_precedences(file:read_line(IODev), Pattern, [], IODev).

parse_precedences({ok, "EOF"}, _Pattern, PrecedenceList, _IODev) ->
    {ok, PrecedenceList};
parse_precedences({ok, Line}, Pattern, PrecedenceList, IODev) ->
    Precedence = parse_precedence(Line, Pattern),
    NewPrecedenceList = [Precedence] ++ PrecedenceList,
    parse_precedences(file:read_line(IODev), Pattern, NewPrecedenceList, IODev).

parse_precedence(Line, Pattern) ->
    {match, [_, A, B | _]} = re:run(Line, Pattern, re_options()),
    {list_to_integer(A), list_to_integer(B)}.

parse_maybe_float(List) ->
    try list_to_integer(List) of
        Value -> Value
    catch
        _:badarg ->
            list_to_float(List)
    end.

% validate_solution
validate_solution(Problem, Solution) ->
    ProblemNodes = dict:fetch_keys(nodedict(Problem)),
    ok = validate_solution_all_nodes(ProblemNodes, Solution),
    ok.

validate_solution_all_nodes(ProblemNodes, Solution) ->
    case lists:sort(ProblemNodes) == lists:sort(Solution) of
        true -> ok;
        _ -> throw(solution_incomplete)
    end.

% grade_slution
grade_solution(Problem, Solution) ->
    EdgeDict = edgedict(Problem),
    SolutionEdges = generate_solution_edges(Solution),
    SolutionLength = solution_length(EdgeDict, SolutionEdges),
    {ok, SolutionLength}.

generate_solution_edges(Solution = [First | _]) ->
    generate_solution_edges(Solution, [], First).

generate_solution_edges([Last], Acc, First) ->
    NewAcc = [{Last, First} | Acc],
    lists:reverse(NewAcc);
generate_solution_edges([From, To | Solution], Acc, First) ->
    NewEdge = {From, To},
    NewAcc = [NewEdge | Acc],
    generate_solution_edges([To | Solution], NewAcc, First).

solution_length(EdgeDict, SolutionEdges) ->
    lists:foldl(
        fun
            (EdgeKey, Acc) ->
                Edge = dict:fetch(EdgeKey, EdgeDict),
                NewAcc = Acc + maps:get(distance, Edge),
                NewAcc
        end,
        0,
        SolutionEdges
    ).
