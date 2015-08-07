-module(erltsp_event).

-export([start_link/0, add_handler/2, delete_handler/2]).
-export([init/4, improvement/4, stop/4, timer/4]).

% gen_event
start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

% api

init(Runtime, Iteration, Length, Solution) ->
    mark(init, Runtime, Iteration, Length, Solution).

improvement(Runtime, Iteration, Length, Solution) ->
    mark(improvement, Runtime, Iteration, Length, Solution).

stop(Runtime, Iteration, Length, Solution) ->
    mark(stop, Runtime, Iteration, Length, Solution).

timer(Runtime, Iteration, Length, Solution) ->
    mark(timer, Runtime, Iteration, Length, Solution).

mark(Mark, Runtime, Iteration, Length, Solution) ->
    gen_event:notify(
        ?MODULE,
        {Mark, Runtime, Iteration, Length, Solution}
     ).
