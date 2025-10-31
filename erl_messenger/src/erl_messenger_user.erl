-module(erl_messenger_user).
-export([register/2, login/2, valid_pair/2]).

register(UsernameBin, PasswordBin) ->
    Username = to_bin(UsernameBin),
    Password = to_bin(PasswordBin),
    ensure_users(),
    case exists_username(Username) of
        true -> {error, username_taken};
        false ->
            Pin = generate_pin(),
            PassHash = crypto:hash(sha256, Password),
            Ts = erlang:system_time(millisecond),
            F = fun() ->
                    mnesia:write({users, Pin, Username, PassHash, Ts})
                end,
            case mnesia:transaction(F) of
                {atomic, ok} -> {ok, #{username => Username, pin => Pin}};
                {aborted, Reason} -> {error, Reason}
            end
    end.

login(UsernameBin, PasswordBin) ->
    Username = to_bin(UsernameBin),
    Password = to_bin(PasswordBin),
    ensure_users(),
    PassHash = crypto:hash(sha256, Password),
    case find_by_username(Username) of
        {ok, {users, Pin, Username, PassHash, _}} -> {ok, #{username => Username, pin => Pin}};
        {ok, _Other} -> {error, bad_credentials};
        not_found -> {error, not_found}
    end.

valid_pair(UsernameBin, PinBin) ->
    Username = to_bin(UsernameBin),
    Pin = to_bin(PinBin),
    ensure_users(),
    case mnesia:dirty_read(users, Pin) of
        [{users, Pin, Username, _H, _Ts}] -> true;
        _ -> false
    end.

exists_username(Username) ->
    case find_by_username(Username) of
        {ok, _R} -> true;
        not_found -> false
    end.

find_by_username(Username) ->
    Sel = [
        {{users, '$1', Username, '$2', '$3'}, [], ['$_']}
    ],
    case catch mnesia:dirty_select(users, Sel) of
        {'EXIT', _} -> not_found;
        [R|_] -> {ok, R};
        [] -> not_found
    end.

generate_pin() ->
    PinBin = <<(rand_hex(4))/binary, "-", (rand_hex(4))/binary>>,
    case catch mnesia:dirty_read(users, PinBin) of
        {'EXIT', _} -> PinBin; %% if table not ready, just use it
        [] -> PinBin;
        _ -> generate_pin()
    end.

rand_hex(N) ->
    BytesNeeded = (N + 1) div 2,
    Bytes = crypto:strong_rand_bytes(BytesNeeded),
    HexList = lists:flatten([io_lib:format("~2.16.0b", [B]) || B <- binary:bin_to_list(Bytes)]),
    HexBin = list_to_binary(HexList),
    binary:part(HexBin, 0, N).

ensure_users() ->
    %% Try to create users table every time; ignore already_exists
    case catch mnesia:create_table(users, [
        {attributes, [pin, username, pass_hash, created_at]},
        {type, set},
        {disc_copies, [node()]},
        {index, [username]}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, users}} -> ok;
        {'EXIT', _} -> ok;
        _ -> ok
    end,
    _ = catch mnesia:wait_for_tables([users], 2000),
    ok.

to_bin(undefined) -> <<>>;
 to_bin(B) when is_binary(B) -> B;
 to_bin(S) when is_list(S) -> list_to_binary(S).