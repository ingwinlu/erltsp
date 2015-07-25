-module(erltsp_event).

-export([start_link/0, add_handler/2, delete_handler/2]).
-export([mark/5]).

% gen_event
start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

% api
mark(Runtime, Iteration, Length, Solution, Mark) ->
    gen_event:notify(
        ?MODULE,
        {mark, Runtime, Iteration, Length, Solution, Mark}
     ).
