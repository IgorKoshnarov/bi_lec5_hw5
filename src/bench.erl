-module(bench).
-export([run/2]).

-export([map_create/2, map_update/2, map_match/2, map_find/2]).
-export([proplist_create/2, proplist_update/2, proplist_match/2, proplist_find/2]).
-export([dict_create/2, dict_update/2, dict_match/2, dict_find/2]).
-export([proc_dict_create/1, proc_dict_update/1, proc_dict_match/1, proc_dict_find/1]).
-export([ets_create/1, ets_update/1, ets_match/1, ets_find/1]).
-export([rec_list_create/2, rec_list_update/2, rec_list_match/2, rec_list_find/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(pair, {key, val}).

run(Elements, Iterations) when is_integer(Elements), is_integer(Iterations), Elements > 0, Iterations > 0 ->
    Pairs = [{X, rand:uniform(Elements)} || X <- lists:seq(1, Elements)],
    RandKeys = [rand:uniform(Elements) || _ <- lists:seq(1, Iterations)],
    RandPairs = [{rand:uniform(Elements), rand:uniform(Elements)} || _ <- lists:seq(1, Iterations)],
    %% benchmark maps
    {MapCreateTime, Map} = timer:tc(bench, map_create, [Pairs, #{}]),
    {MapMatchTime, _MapMatches} = timer:tc(bench, map_match, [RandKeys, Map]),
    {MapUpdateTime,_Map1} = timer:tc(bench, map_update, [RandPairs, Map]),
    {MapFindTime, _MapValues} = timer:tc(bench, map_find, [RandKeys, Map]), 
    %% benchmark proplists
    {ProplistCreateTime, Proplist} = timer:tc(bench, proplist_create, [Pairs, []]),
    {ProplistMatchTime, _ProplistMatches} = timer:tc(bench, proplist_match, [RandKeys, Proplist]),
    {ProplistUpdateTime, _Proplist1} = timer:tc(bench, proplist_update, [RandPairs, Proplist]),
    {ProplistFindTime, _ProplistValues} = timer:tc(bench, proplist_find, [RandKeys, Proplist]), 
    %% benchmark dict
    {DictCreateTime, Dict} = timer:tc(bench, dict_create, [Pairs, dict:new()]),
    {DictMatchTime, _DictMatches} = timer:tc(bench, dict_match, [RandKeys, Dict]),
    {DictUpdateTime, _Dict1} = timer:tc(bench, dict_update, [RandPairs, Dict]),
    {DictFindTime, _DictValues} = timer:tc(bench, dict_find, [RandKeys, Dict]), 
    %% benchmark process dictionary
    proc_dict_clean(Elements), %% clean only numeric keys from process dictionary
    {ProcDictCreateTime, _ProcDict} = timer:tc(bench, proc_dict_create, [Pairs]),
    {ProcDictMatchTime, _ProcDictMatches} = timer:tc(bench, proc_dict_match, [RandKeys]),
    {ProcDictUpdateTime, _ProcDict1} = timer:tc(bench, proc_dict_update, [RandPairs]),
    {ProcDictFindTime, _ProcDictValues} = timer:tc(bench, proc_dict_find, [RandKeys]),
    %% benchmark ets
    %% clean up the previously created table
    case ets:whereis(numbers) of
        undefined -> ets:new(numbers, [named_table]);
        _ -> ets:delete(numbers), ets:new(numbers, [named_table])
    end,
    {EtsCreateTime, _Ets} = timer:tc(bench, ets_create, [Pairs]),
    {EtsMatchTime, _EtsMatches} = timer:tc(bench, ets_match, [RandKeys]),
    {EtsUpdateTime, _Ets1} = timer:tc(bench, ets_update, [RandPairs]),
    {EtsFindTime, _EtsValues} = timer:tc(bench, ets_find, [RandKeys]),    
    %% benchmark list of records
    {RecListCreateTime, RecList} = timer:tc(bench, rec_list_create, [Pairs, []]),
    {RecListMatchTime, _RecListMatches} = timer:tc(bench, rec_list_match, [RandKeys, RecList]),
    {RecListUpdateTime, _RecList1} = timer:tc(bench, rec_list_update, [RandPairs, RecList]),
    {RecListFindTime, _RecListValues} = timer:tc(bench, rec_list_find, [RandKeys, RecList]),
    io:format("Elts: ~-7.10B| Iter: ~-4.10B| ~10s | ~10s | ~10s | ~10s | ~10s | ~10s~n", 
    [Elements, Iterations, maps, proplists, dict, proc_dict, etc, records]),
    io:format("~102c~n", [$-]),
    io:format("create, N = ~-7.10B      | ~10.10B | ~10.10B | ~10.10B | ~10.10B | ~10.10B | ~10.10B~n", 
    [Elements, MapCreateTime, ProplistCreateTime, DictCreateTime, ProcDictCreateTime, EtsCreateTime, RecListCreateTime]),
    io:format("update, N = ~-7.10B      | ~10.10B | ~10.10B | ~10.10B | ~10.10B | ~10.10B | ~10.10B~n", 
    [Iterations, MapUpdateTime, ProplistUpdateTime, DictUpdateTime, ProcDictUpdateTime, EtsUpdateTime, RecListUpdateTime]),
    io:format("patternmatch, N = ~-7.10B| ~10.10B | ~10.10B | ~10.10B | ~10.10B | ~10.10B | ~10.10B~n", 
    [Iterations, MapMatchTime, ProplistMatchTime, DictMatchTime, ProcDictMatchTime, EtsMatchTime, RecListMatchTime]),
    io:format("function, N = ~-7.10B    | ~10.10B | ~10.10B | ~10.10B | ~10.10B | ~10.10B | ~10.10B~n", 
    [Iterations, MapFindTime, ProplistFindTime, DictFindTime, ProcDictFindTime, EtsFindTime, RecListFindTime]),
    ok.

%% map 

map_create([{Key, Val} | T], Map) ->
    map_create(T, maps:put(Key, Val, Map));
map_create([], Map) ->
    Map.

map_update([{Key, Val} | T], Map) ->
    map_update(T, maps:update(Key, Val, Map));
map_update([], Map) ->
    Map.

map_match(Keys, Map) ->
    map_match(Keys, Map, []).

map_match([Key | T], Map, Acc) ->
    #{Key := Val} = Map,
    map_match(T, Map, [{Key, Val} | Acc]);
map_match([], _Map, Acc) ->
    Acc.

map_find(Keys, Map) ->
    map_find(Keys, Map, []).

map_find([Key | T], Map, Acc) ->
    map_find(T, Map, [maps:get(Key, Map) | Acc]);
map_find([], _Map, Acc) ->
    Acc.

%% proplist

proplist_create([{Key, Val} | T], Acc) ->
    proplist_create(T, [{Key, Val} | Acc]);
proplist_create([], Acc) ->
    Acc.

proplist_update([{Key, Val} | T], List) ->
    proplist_update(T, lists:keyreplace(Key, 1, List, {Key, Val}));
proplist_update([], List) ->
    List.

proplist_match(Keys, List) ->
    proplist_match(Keys, List, List, []).


proplist_match([Key | T], List, [{Key, Val} | _Rest], Acc) ->
    proplist_match(T, List, List, [Val | Acc]);
proplist_match([Key | T], List, [_H | Rest], Acc) ->
    proplist_match([Key | T], List, Rest, Acc);
proplist_match([_Key | T], List, [], Acc) ->
    proplist_match(T, List, List, Acc);
proplist_match([], List, List, Acc) ->
    Acc.

proplist_find(Keys, List) ->
    proplist_find(Keys, List, []).

proplist_find([Key | T], List, Acc) ->
    proplist_find(T, List, [proplists:lookup(Key, List) | Acc]);
proplist_find([], _List, Acc) ->
    Acc.

%% dict

dict_create([{Key, Val} | T], Dict) ->
    dict_create(T, dict:store(Key, Val, Dict));
dict_create([], Dict) ->
    Dict.

dict_update([{Key, Val} | T], Dict) ->
    dict_update(T, dict:update(Key, fun(_X) -> Val end, Dict));
dict_update([], Dict) ->
    Dict.

dict_match(Keys, Dict) ->
    dict_match(Keys, Dict, dict:to_list(Dict), []).

dict_match([Key | T], Dict, [{Key, Val} | _Rest], Acc) ->
    dict_match(T, Dict, dict:to_list(Dict), [Val | Acc]);
dict_match([Key | T], Dict, [_H | Rest], Acc) ->
    dict_match([Key | T], Dict, Rest, Acc);
dict_match([_Key | T], Dict, [], Acc) ->
    dict_match(T, Dict, dict:to_list(Dict), Acc);
dict_match([], _Dict, _List, Acc) ->
    Acc.

dict_find(Keys, Dict) ->
    dict_find(Keys, Dict, []).

dict_find([Key | T], Dict, Acc) ->
    dict_find(T, Dict, [dict:fetch(Key, Dict) | Acc]);
dict_find([], _Dict, Acc) ->
    Acc.

%% process dictionary

proc_dict_clean(0) -> 
    ok;
proc_dict_clean(Count) ->
    erase(Count),
    proc_dict_clean(Count - 1).

proc_dict_create([{Key, Val} | T]) ->
    put(Key, Val),
    proc_dict_create(T);
proc_dict_create([]) ->
    get().

proc_dict_update([{Key, Val} | T]) ->
    put(Key, Val),
    proc_dict_update(T);
proc_dict_update([]) ->
    get().

proc_dict_match(Keys) ->
    proc_dict_match(Keys, []).

proc_dict_match(Keys, Acc) ->
    proc_dict_match(Keys, get(), Acc).

proc_dict_match([Key | T], [{Key, Val} | _Rest], Acc) ->
    proc_dict_match(T, get(), [{Key, Val} | Acc]);
proc_dict_match([Key | T], [_H | Rest], Acc) ->
    proc_dict_match([Key | T], Rest, Acc);
proc_dict_match([_Key | T], [], Acc) ->
    proc_dict_match(T, get(), Acc);
proc_dict_match([], _, Acc) ->
    Acc.

proc_dict_find(Keys) ->
    proc_dict_find(Keys, []).

proc_dict_find([Key | T], Acc) ->
    Val = get(Key),
    case Val of
        undefined -> proc_dict_find(T, Acc);
        _ -> proc_dict_find(T, [Val | Acc])
    end;

proc_dict_find([], Acc) ->
    Acc.

%% ets

ets_create([Pair | T]) ->
    ets:insert(numbers, Pair),
    ets_create(T);
ets_create([]) -> ok.

ets_update([Pair | T]) ->
    ets:insert(numbers, Pair),
    ets_update(T);
ets_update([]) -> ok.

ets_match(Keys) ->
    ets_match(Keys, []).

ets_match([Key | T], Acc) ->
    Res = ets:select(numbers, [{{'$1', '$2'}, [{'=:=', '$1', Key}], ['$2']}]),
    case Res of
        [V] -> ets_match(T, [V | Acc]);
        [] -> ets_match(T, Acc)
    end;
ets_match([], Acc) ->
    Acc.


ets_find(Keys) ->
    ets_find(Keys, []).

ets_find([Key | T], Acc) ->
    case ets:lookup(numbers, Key) of
        [{_K, V}] -> ets_find(T, [V | Acc]);
        [] -> ets_find(T, Acc)
    end;
ets_find([], Acc) ->
    Acc.

%% record

rec_list_create([{Key, Val} | T], Acc) ->
    rec_list_create(T, [#pair{key = Key, val = Val} | Acc]);
rec_list_create([], Acc) ->
    Acc.

rec_list_update([{Key, Val} | T], RecList) ->
    rec_list_update(T, lists:keyreplace(Key, 2, RecList, #pair{key = Key, val = Val}));
rec_list_update([], Acc) ->
    Acc.

rec_list_match(Keys, RecList) ->
    rec_list_match(Keys, RecList, []).

rec_list_match(Keys, RecList, Acc) ->
    rec_list_match(Keys, RecList, RecList, Acc).

rec_list_match([Key | T], RecList, [#pair{key = Key, val = Val} | _Rest], Acc) ->
    rec_list_match(T, RecList, RecList, [Val | Acc]);
rec_list_match([Key | T], RecList, [_H | Rest], Acc) ->
    rec_list_match([Key | T], RecList, Rest, Acc);
rec_list_match([_Key | T], RecList, [], Acc) ->
    rec_list_match(T, RecList, RecList, Acc);
rec_list_match([], _RecList, _RecList, Acc) ->
    Acc.

rec_list_find(Keys, RecList) ->
    rec_list_find(Keys, RecList, []).

rec_list_find([Key | T], RecList, Acc) ->
    case lists:keyfind(Key, 2, RecList) of
        #pair{key = Key, val = Val} -> rec_list_find(T, RecList, [Val | Acc]);
        false -> rec_list_find(T, RecList, Acc)
    end;
rec_list_find([], _RecList, Acc) ->
    Acc.

-ifdef(TEST).

bench_test_() -> [
    {"bench:run works", ?_assertEqual(test(), ok)}
].

test() ->
    bench:run(1000, 20).

-endif.