-module(erl_messenger_register_h).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, _State) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"OPTIONS">> ->
            Cors = cors_headers(),
            Req2 = cowboy_req:reply(204, Cors, <<>>, Req),
            {ok, Req2, undefined};
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req),
            case safe_decode_json(Body) of
                {ok, Map} ->
                    Username = maps:get(<<"username">>, Map, <<>>),
                    Password = maps:get(<<"password">>, Map, <<>>),
                    case erl_messenger_user:register(Username, Password) of
                        {ok, U} -> json_reply(200, U, Req1);
                        {error, username_taken} -> json_reply(409, #{error => <<"username_taken">>}, Req1);
                        {error, Reason} -> json_reply(500, #{error => to_bin(Reason)}, Req1)
                    end;
                {error, _} -> json_reply(400, #{error => <<"bad_json">>}, Req1)
            end;
        _ ->
            Req2 = cowboy_req:reply(405, cors_headers(), <<"method_not_allowed">>, Req),
            {ok, Req2, undefined}
    end.

safe_decode_json(Body) when is_binary(Body) ->
    try jsx:decode(Body, [return_maps]) of
        M when is_map(M) -> {ok, M};
        _ -> {error, bad_format}
    catch _:_ -> {error, decode_failed} end.

json_reply(Status, Obj, Req) ->
    Json = jsx:encode(Obj),
    Hdrs = maps:merge(#{<<"content-type">> => <<"application/json">>}, cors_headers()),
    Req2 = cowboy_req:reply(Status, Hdrs, Json, Req),
    {ok, Req2, undefined}.

cors_headers() ->
    #{
      <<"access-control-allow-origin">> => <<"*">>,
      <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
      <<"access-control-allow-headers">> => <<"content-type, accept">>
    }.

to_bin(B) when is_binary(B) -> B;
 to_bin(S) when is_list(S) -> list_to_binary(S);
 to_bin(Other) -> list_to_binary(io_lib:format("~p", [Other])).