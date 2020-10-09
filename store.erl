-module(store).
-export([create/0, add/3, lookup/2, split/2, merge/2,size/1]).
%create a new store
create() ->
    [].
%add a key value pair, return the updated store
add(Key, Value, Store) ->
    lists:keystore(Key, 1, Store, {Key, Value}).

% return a tuple {Key, Value} or the atom false
lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

%return a tuple {Updated, Rest} where the
%updated store only contains the key value pairs requested and the rest
%are found in a list of key-value pairs
split(Key, Store) ->
    Sorted = lists:keysort(1, Store),
    lists:splitwith(fun(E) -> E =< Key end, Sorted).
% add a list of key-value pairs to a store
merge(ToMerge, Store) ->
    Store ++ ToMerge.

size(Store) ->
	length(Store).