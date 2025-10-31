-module(erl_messenger_ws_h).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, _Opts) ->
    Username0 = cowboy_req:qs_val(<<"username">>, Req),
    Pin0 = cowboy_req:qs_val(<<"pin">>, Req),
    Username = ensure_bin(Username0),
    Pin = ensure_bin(Pin0),
    case erl_messenger_user:valid_pair(Username, Pin) of
        true ->
            ets:insert(erl_messenger_clients, {Pin, self()}),
            State = #{username => Username, pin => Pin},
            {cowboy_websocket, Req, State};
        false ->
            Req2 = cowboy_req:reply(403, #{}, <<"invalid_credentials">>, Req),
            {ok, Req2, undefined}
    end.

websocket_init(State) ->
    {reply, {text, <<"Connecté au serveur">>}, State}.

websocket_handle({text, Msg}, State=#{username := Username, pin := Pin}) ->
    %% Broadcast to all clients under same PIN, including sender
    broadcast(Pin, <<"Serveur: ", Username/binary, " #", Pin/binary, ": ", Msg/binary>>),
    {ok, State};
websocket_handle(_Other, State) ->
    {ok, State}.

websocket_info({send, Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, State) ->
    case maps:get(pin, State, undefined) of
        undefined -> ok;
        Pin -> ets:delete_object(erl_messenger_clients, {Pin, self()})
    end,
    ok.

broadcast(Pin, Msg) ->
    case ets:lookup(erl_messenger_clients, Pin) of
        [] -> ok;
        Es -> lists:foreach(fun({Pin, Pid}) -> Pid ! {send, Msg} end, Es)
    end.

ensure_bin(undefined) -> <<>>;
ensure_bin(B) when is_binary(B) -> B;
ensure_bin(S) when is_list(S) -> list_to_binary(S);
ensure_bin(Other) -> io_lib:format("~p", [Other]).