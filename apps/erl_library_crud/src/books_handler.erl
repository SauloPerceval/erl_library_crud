-module(books_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allow_missing_post/2]).
-export([get_to_json/2]).
-export([post_from_json/2]).
-export([patch_from_json/2]).
-export([delete_resource/2]).

init(Req0, Opts) ->
    {cowboy_rest, Req0, Opts}.


allowed_methods(Req, State) ->
    case cowboy_req:binding(id, Req) of
        undefined ->
            {[<<"GET">>, <<"POST">>], Req, State};
        _ ->
            {[<<"GET">>, <<"PATCH">>, <<"DELETE">>], Req, State}
    end.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, get_to_json}], Req, State}.


content_types_accepted(Req, State) ->
        case cowboy_req:method(Req) of
        <<"POST">> ->
            {[{<<"application/json">>, post_from_json}], Req, State};
        <<"PATCH">> ->
            {[{<<"application/json">>, patch_from_json}], Req, State}
    end.
    

allow_missing_post(Req, State) ->
    {true, Req, State}.


delete_resource(Req, State) ->
    Id = cowboy_req:binding(id, Req),
    if Id == undefined ->
        {false, Req, State};
    true ->
        db_executor ! {delete, binary_to_integer(Id), self()},
        receive
            _ -> 
                {true, Req, State}
        after 1000 ->
                {false, Req, State}
        end
    end.


get_to_json(Req, State) ->
    Id = cowboy_req:binding(id, Req),
    if 
        Id == undefined ->
            db_executor ! {get_all, self()},
            {Ok, Body} = fetch_result(true),
            if 
                Ok ->
                    {Body, Req, State};
                true ->
                    Req1 = cowboy_req:reply(204, Req),
                    {<<"[]">>, Req1, State}
            end;
        true ->
            get_to_json(Id, Req, State)
    end.


get_to_json(Id, Req, State) ->
    Id = cowboy_req:binding(id, Req),
    db_executor ! {get, binary_to_integer(Id), self()},

    {Ok, Body} = fetch_result(false),
    if 
        Ok ->
            {Body, Req, State};
        true ->
            Req1 = cowboy_req:reply(404, Req),
            {<<"">>, Req1, State}
    end.


post_from_json(Req0, State) ->
    {BinReqBody, Req1} = read_req_body(Req0),
    ReqBody = jsx:decode(BinReqBody),

    db_executor ! {create, ReqBody, self()},

    {Ok, Body} = fetch_result(false),
    if 
        Ok ->
            Req2 = cowboy_req:set_resp_body(Body, Req1);
        true ->
            Req2 = cowboy_req:reply(400, Req1)
    end,
    {true, Req2, State}.



patch_from_json(Req0, State) ->
    Id = cowboy_req:binding(id, Req0),
    {BinReqBody, Req1} = read_req_body(Req0),
    
    db_executor ! {update, binary_to_integer(Id), jsx:decode(BinReqBody), self()},

    {Ok, Body} = fetch_result(false),
    if 
        Ok ->
            Req2 = cowboy_req:set_resp_body(Body, Req1);
        true ->
            Req2 = cowboy_req:reply(400, Req1)
    end,
    {true, Req2, State}.

read_req_body(Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {<<Data/binary>>, Req};
        {more, Data, Req} -> read_req_body(Req, <<Data/binary>>)
    end.

read_req_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {<< Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_req_body(Req, << Acc/binary, Data/binary >>)
    end.


fetch_result(Multiple)->
    receive
        Result -> Result,
        fetch_result(Multiple, Result)
    after 1000 ->
        {false, null}
    end.


build_result_map({Id, Title, Author}) ->
    #{<<"id">> => Id, <<"title">> => Title, <<"author">> => Author}.


fetch_result(Multiple, ResultRows)->
    case length(ResultRows) of
        0 ->
            {false, null};
        _ ->
            if 
                Multiple ->
                    ResultMap = lists:map(fun(Row) -> build_result_map(Row) end, ResultRows);
                true ->
                    ResultItem = lists:nth(1, ResultRows),
                    ResultMap = build_result_map(ResultItem)
                end,
            Body = jsx:encode(ResultMap),
            {true, Body}
    end.
