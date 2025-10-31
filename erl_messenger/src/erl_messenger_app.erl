%%%-------------------------------------------------------------------
%% @doc erl_messenger public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_messenger_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:ensure_all_started(cowboy),
    ensure_mnesia(),
    ensure_clients_table(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", erl_messenger_health_h, []},
            {"/register", erl_messenger_register_h, []},
            {"/login", erl_messenger_login_h, []},
            {"/ws", erl_messenger_ws_h, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(erl_messenger_http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    erl_messenger_sup:start_link().

stop(_State) ->
    ok.

ensure_clients_table() ->
    case ets:info(erl_messenger_clients) of
        undefined -> ets:new(erl_messenger_clients, [bag, public, named_table]);
        _ -> ok
    end.

ensure_mnesia() ->
    %% Pin mnesia directory to project for consistency across shells
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join([Cwd, "Mnesia"]),
    _ = catch filelib:ensure_dir(filename:join(Dir, "dummy")),
    ok = application:set_env(mnesia, dir, Dir),
    %% Create schema and start mnesia (idempotent)
    _ = catch mnesia:create_schema([node()]),
    ok = mnesia:start(),
    %% Create table if missing; ignore already_exists
    _ = catch mnesia:create_table(users, [
        {attributes, [pin, username, pass_hash, created_at]},
        {type, set},
        {disc_copies, [node()]},
        {index, [username]}
    ]),
    %% Ensure table is loaded before accepting requests
    case mnesia:wait_for_tables([users], 5000) of
        ok -> ok;
        {timeout, _} -> ok;
        {error, _} -> ok
    end.

create_users() ->
    mnesia:create_table(users, [
        {attributes, [pin, username, pass_hash, created_at]},
        {type, set},
        {disc_copies, [node()]},
        {index, [username]}
    ]).
