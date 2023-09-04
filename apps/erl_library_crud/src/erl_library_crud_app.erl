%%%-------------------------------------------------------------------
%% @doc erl_library_crud public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_library_crud_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/books", books_handler, []},
            {"/books/:id", books_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(crud_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    erl_library_crud_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
