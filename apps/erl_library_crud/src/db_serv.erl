-module(db_serv).
-export([start_link/0, init/1]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    register(db_executor, spawn(db_executor, db_receiver, [])),
    {ok, self()}.

