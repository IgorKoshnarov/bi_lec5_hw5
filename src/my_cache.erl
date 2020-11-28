-module(my_cache).
-export([create/1, insert/4, lookup/2, delete_obsolete/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("../include/my_cache.hrl").

create(TableName) ->
    ets:new(TableName, [named_table, {keypos, #kv.key}]),
    ok.

insert(TableName, Key, Value, Expire) ->
    ExpTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Expire,
    ets:insert(TableName, #kv{key = Key, value = Value, expire = ExpTime}),
    ok.

lookup(TableName, Key) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    case ets:lookup(TableName, Key) of
        [#kv{value = Value} = El] when El#kv.expire >= Now -> {ok, Value};
        _ -> undefined
    end.

delete_obsolete(TableName) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    ets:select_delete(TableName, [{#kv{expire = '$1', _ = '_'}, [{'<', '$1', Now}], [true]}]),
    ok.

-ifdef(TEST).

my_cache_test_() -> [
    {"my_cache stores data in ets table", ?_assertEqual(test(), ok)}
].

test() ->
    my_cache:create(cache),
    [my_cache:insert(cache, X, X, 1) || X <- lists:seq(1, 10)],
    [my_cache:insert(cache, X, X, 60) || X <- lists:seq(11, 20)],
    List = lists:seq(1, 20),
    List = [element(2, my_cache:lookup(cache, X)) || X <- lists:seq(1, 20)],
    timer:sleep(2000),
    List1 = [case X of X when (X < 11) -> undefined; X -> X end || X <- lists:seq(1, 20)],
    List1 = [case my_cache:lookup(cache, X) of {ok, Value} -> Value; undefined -> undefined end || X <- lists:seq(1, 20)],
    my_cache:delete_obsolete(cache),
    10 = length(ets:select(cache, [{#kv{key = '$1', _ = '_'}, [], ['$_']}])),
    ok.


-endif.