-module(tsp_event).

-export([start_link/0, add_handler/2, delete_handler/2]).
-export([load_problem/1]).

% gen_event
start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

% api
load_problem(Problem) ->
    gen_event:notify(?MODULE, {load, Problem}).
