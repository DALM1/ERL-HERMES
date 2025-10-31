-module(erl_messenger_health_h).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, _State) ->
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"ok">>, Req),
    {ok, Req2, undefined}.