-module(tsp_problem).

-export([from_file/1]).

% PATTERNS

pattern_dimension() -> re:compile("DIMENSION:\\\s+([0-9]+)").
pattern_threshold() -> re:compile("THRESHOLD:\\\s+([0-9]+(\\\.[0-9]+)?)").
pattern_node() -> re:compile("\\\s*([0-9]+)\\s+([0-9]+(\\\.[0-9]+)?)\\s+([0-9]+(\\\.[0-9]+)?)").
pattern_precedence() -> re:compile("\\\s*([0-9]+)\\\s+([0-9]+)").

re_options() -> [
    {capture, all, list}
].

% API

from_file(File) ->
    error_logger:info_msg("loading Problem from " ++ File),
    {ok, IODev} = file:open(File, [read, read_ahead]),
    {ok, Dimension} = parse_dimension(IODev),
    {ok, Threshold} = parse_threshold(IODev),
    {ok, "NODE_COORD_SECTION\n"} = file:read_line(IODev),
    {ok, NodeList} = parse_nodes(IODev, Dimension),
    {ok, "PRECEDENCE_SECTION\n"} = file:read_line(IODev),
    {ok, PrecedenceList} = parse_precedences(IODev),
    #{
        file => File,
        dimension => Dimension,
        threshold => Threshold,
        nodes => NodeList,
        precedences => PrecedenceList
    }.



% PRIV

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
