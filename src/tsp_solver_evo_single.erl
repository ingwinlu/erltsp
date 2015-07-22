-module(tsp_solver_evo_single).

-export([init/1, iterate/1, best/1]).

-behaviour(tsp_solver).

-record(state, {
    problem,
    population
}).

-define(POPULATION_SIZE, 100).
-define(NB_SIZE, 10).
-define(MUTATION_CHANCE, 0.10).

% API
init(Problem) ->
    ok = setup_randomness(),
    Population = init_population(Problem),
    {
        ok,
        #state{problem = Problem, population=Population}
    }.

iterate(State = #state{population=Population}) ->
    {ok, Selection} = selection(Population),
    {ok, Recombination} = recombination(Selection),
    {ok, Mutation} = mutation(Recombination),
    {ok, update_state(State, Mutation)}.

best(#state{population=Population}) ->
    {ok, get_best(Population)}.

% update
update_state(State = #state{problem = Problem, population=Population}, Mutation) ->
    {ok, Length} = tsp_problem:solution(Problem, Mutation),
    NewPopulation = maybe_update_population(Population, {Length, Mutation}),
    State#state{population=NewPopulation}.

maybe_update_population(Population, NewSolution) ->
    try validate_update(Population, NewSolution) of
        ok -> update_population(Population, NewSolution)
    catch
        _:_ -> Population
    end.

validate_update(Population, NewSolution) ->
    ok = validate_update_new_key_is_unique(Population, NewSolution),
    ok = validate_new_solution_is_improvement(Population, NewSolution),
    ok.

validate_update_new_key_is_unique(Population, {NewKey, _}) ->
    case gb_trees:is_defined(NewKey, Population) of
        true -> throw(population_key_exists);
        false -> ok
    end.

validate_new_solution_is_improvement(Population, {NewKey, _}) ->
    {OldKey, _} = gb_trees:largest(Population),
    case NewKey < OldKey of
        true -> ok;
        false -> throw(not_an_improvement)
    end.

update_population(Population, {NewKey, NewSolution}) ->
    {_, _, Population1} = gb_trees:take_largest(Population),
    gb_trees:insert(NewKey, NewSolution, Population1).

% selection
selection(Population) ->
    selection(Population, ?NB_SIZE).

selection(Population, SelectionSize) ->
    PopulationKeys = gb_trees:keys(Population),
    NB = generate_nb(PopulationKeys, SelectionSize),
    Picks = pick_best_from_nb(NB),
    Selection = lists:map(
        fun
            (Key) ->
                gb_trees:get(Key, Population)
        end,
        Picks
    ),
    {ok, Selection}.

generate_nb(Keys, Amount) ->
    RKeys = randomize_list(Keys),
    {Selection, _} = lists:split(Amount, RKeys),
    Selection.

pick_best_from_nb(NB) ->
    {Best, _} = lists:split(2, lists:sort(NB)),
    Best.

% recombination
recombination(Parents) ->
    Edges = generate_edges(Parents),
    StartPoint = get_starting_point(Edges),
    add_new_edge(Edges, [StartPoint]).

generate_edges(Parents) ->
    generate_edges(Parents, dict:new()).

generate_edges([], EdgeDict) ->
    EdgeDict;
generate_edges([Parent | Parents], EdgeDict) ->
    EdgeDict1 = generate_edges_1(Parent, EdgeDict),
    generate_edges(Parents, EdgeDict1).

generate_edges_1(Solution = [Head | _], EdgeDict) ->
    generate_edges_1(Solution, Head, EdgeDict).

generate_edges_1([From], Head, EdgeDict) ->
    {ok, EdgeDict1} = append_edges(From, Head, EdgeDict),
    EdgeDict1;
generate_edges_1([From, To | Tail], Head, EdgeDict) ->
    {ok, EdgeDict1} = append_edges(From, To, EdgeDict),
    generate_edges_1([To | Tail], Head, EdgeDict1).

append_edges(From, To, EdgeDict) ->
    EdgeDict1 = dict:append(From, To, EdgeDict),
    EdgeDict2 = dict:append(To, From, EdgeDict1),
    {ok, EdgeDict2}.

get_starting_point(EdgeDict) ->
    random:uniform(dict:size(EdgeDict)).

add_new_edge(Edges, Acc = [LastNode | _]) ->
    Targets = dict:fetch(LastNode, Edges),
    TargetsRandom = randomize_list(Targets),
    add_new_edge_unique(Edges, Acc, TargetsRandom).

add_new_edge_unique(Edges, Acc, []) ->
    EdgeCount = dict:size(Edges),
    Targets = randomize_list(lists:seq(1, EdgeCount)),
    add_new_edge_unique(Edges, Acc, Targets);
add_new_edge_unique(Edges, Acc, [MaybeNewEdge | RecombinationTargets]) ->
    add_new_edge_unique(Edges, Acc, RecombinationTargets,
                        MaybeNewEdge, lists:member(MaybeNewEdge, Acc)).

add_new_edge_unique(Edges, Acc, RecombinationTargets, _, true) ->
    add_new_edge_unique(Edges, Acc, RecombinationTargets);
add_new_edge_unique(Edges, Acc, _, NewEdge, false) ->
    NewAcc = [NewEdge | Acc],
    check_complete_solution(Edges, NewAcc).

check_complete_solution(Edges, MaybeSolution) ->
    SizeEdges = dict:size(Edges),
    SizeSolution = erlang:length(MaybeSolution),
    case SizeEdges =:= SizeSolution of
        true -> {ok, lists:reverse(MaybeSolution)};
        false -> add_new_edge(Edges, MaybeSolution)
    end.

% mutation
mutation(Solution) ->
    mutation(Solution, ?MUTATION_CHANCE).

mutation(Solution, Chance) ->
    maybe_mutate(random:uniform(), Chance, Solution).

maybe_mutate(R, C, Solution) when R < C ->
    do_mutate(Solution);
maybe_mutate(_, _, Solution) ->
    {ok, Solution}.

do_mutate(Solution) ->
    SolutionLength = erlang:length(Solution),
    MutationLocation = random:uniform(SolutionLength-1),
    do_mutate_1(lists:split(MutationLocation, Solution)).

do_mutate_1({H, [A, B | T]}) ->
    concat_mutation(H, T, A, B);
do_mutate_1({[B | H], [A | T]}) ->
    concat_mutation(H, T, A, B).

concat_mutation(H, T, A, B) ->
    {ok, H ++ [B] ++ [A] ++ T}.

% setup, helpers
setup_randomness() ->
    application:ensure_all_started(crypto),
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A, B, C}),
    ok.

init_population(Problem) ->
    init_population(Problem, ?POPULATION_SIZE, gb_trees:empty()).

init_population(_Problem, 0, Tree) ->
    Tree;
init_population(Problem, ToGenerate, Tree) ->
    NodeDict = tsp_problem:nodedict(Problem),
    Nodes = dict:fetch_keys(NodeDict),
    Solution = gen_solution(Nodes),
    {ok, Length} = tsp_problem:solution(Problem, Solution),
    case gb_trees:lookup(Length, Tree) of
        none -> 
            NewTree = gb_trees:insert(Length, Solution, Tree),
            init_population(Problem, ToGenerate-1, NewTree);
        _ ->
            init_population(Problem, ToGenerate, Tree)
    end.

get_best(Population) ->
    gb_trees:smallest(Population).

gen_solution(Nodes) ->
    randomize_list(Nodes).

randomize_list(List) ->
    [X || 
        {_, X} <- lists:sort([{random:uniform(), N} || 
                              N <- List
                             ])
    ].
