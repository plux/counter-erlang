%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc counter - counting made easy
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(counter).

%%%_* Exports ==========================================================
-export([count/1]).
-export([count_with/2]).
-export([from_list/1]).
-export([incr/2, incr/3]).
-export([add/2]).
-export([subtract/2]).
-export([dups/1]).
-export([unique/1]).
-export([max/1]).
-export([most_common/2]).
-export([least_common/2]).
-export([min/1]).
-export([elements/1]).
-export([union/2]).
-export([intersection/2]).
-export([only_positive/1]).
-export([sum/1]).

-export_type([counter/0, counter/1]).

%%%_* Types ============================================================
-type counter(E)      :: #{E => amount()}.
-type counter()       :: counter(elem()).
-type counter_list(E) :: [{E, amount()}].
-type amount()        :: integer().
-type elem()          :: any().
-type count_fun()     :: fun((_) -> {elem(), amount()}).

%%%_* API ==============================================================
-spec count([A]) -> counter(A).
count(L) when is_list(L) ->
    lists:foldl(fun(Key, Counter) -> incr(Counter, Key) end, #{}, L).

-spec count_with(count_fun(), [any()]) -> counter().
count_with(F, L) when is_function(F), is_list(L) ->
    from_list(lists:map(F, L)).

-spec from_list(counter_list(A)) -> counter(A).
from_list(L) when is_list(L) ->
    lists:foldl(fun({Key, Amount}, Acc) -> incr(Acc, Key, Amount) end, #{}, L).

-spec incr(counter(A), B) -> counter(A|B).
incr(Counter, Key) ->
    incr(Counter, Key, 1).

-spec incr(counter(A), B, amount()) -> counter(A|B).
incr(Counter, Key, Amount) ->
    maps:update_with(Key, fun(Count) -> Count + Amount end, Amount, Counter).

-spec add(counter(A), counter(B)) -> counter(A|B).
add(C1, C2) when is_map(C1), is_map(C2) ->
    maps:fold(fun(Key, Value, Acc) -> incr(Acc, Key, Value) end, C1, C2).

-spec subtract(counter(A), counter(B)) -> counter(A|B).
subtract(C1, C2) when is_map(C1), is_map(C2) ->
    maps:fold(fun(Key, Value, Acc) -> incr(Acc, Key, -Value) end, C1, C2).

-spec dups(counter(A)) -> counter(A).
dups(Counter) when is_map(Counter) ->
    maps:filter(fun(_, V) -> V > 1 end, Counter).

-spec unique(counter(A)) -> [A].
unique(Counter) when is_map(Counter) ->
    maps:keys(maps:filter(fun(_, V) -> V =:= 1 end, Counter)).

-spec max(counter(A)) -> {A, amount()}.
max(Counter) when is_map(Counter) ->
    [H|T] = maps:to_list(Counter),
    lists:foldl(fun({K, V}, {_, MaxV}) when V > MaxV -> {K, V};
                   (_, Acc)                          -> Acc
                end, H, T).

-spec most_common(counter(A), pos_integer()) -> counter_list(A).
most_common(Counter, N) ->
    L = maps:to_list(Counter),
    {MostCommon, _} = lists:split(N, lists:reverse(lists:keysort(2, L))),
    MostCommon.

-spec least_common(counter(A), pos_integer()) -> counter_list(A).
least_common(Counter, N) ->
    L = maps:to_list(Counter),
    {MostCommon, _} = lists:split(N, lists:keysort(2, L)),
    MostCommon.

-spec min(counter(A)) -> {A, amount()}.
min(Counter) when is_map(Counter) ->
    [H|T] = maps:to_list(Counter),
    lists:foldl(fun({K,V}, {_, MaxV}) when V < MaxV -> {K, V};
                   (_, Acc)                         -> Acc
                end, H, T).

-spec elements(counter(A)) -> [A].
elements(Counter) when is_map(Counter) ->
    do_elements(maps:to_list(Counter)).

-spec union(counter(A), counter(B)) -> counter(A|B).
union(C1, C2) ->
    lists:foldl(fun(K, Acc) ->
                        case {C1, C2} of
                            {#{K := V1}, #{K := V2}} -> Acc#{K => max(V1, V2)};
                            {#{K := V1}, _}          -> Acc#{K => V1};
                            {_, #{K := V2}}          -> Acc#{K => V2}
                        end
                end, #{}, lists:usort(maps:keys(C1) ++ maps:keys(C2))).

-spec intersection(counter(A), counter(B)) -> counter(A|B).
intersection(C1, C2) ->
    lists:foldl(fun(K, Acc) ->
                        case {C1, C2} of
                            {#{K := V1}, #{K := V2}} -> Acc#{K => min(V1, V2)};
                            {_, _}                   -> Acc
                        end
                end, #{}, lists:usort(maps:keys(C1) ++ maps:keys(C2))).

-spec only_positive(counter(A)) -> counter(A).
only_positive(Counter) ->
    maps:filter(fun(_, V) -> V > 0 end, Counter).

-spec sum(counter()) -> amount().
sum(Counter) ->
    lists:sum(maps:values(Counter)).

%%%_* Internal =========================================================
do_elements([]) ->
    [];
do_elements([{K,V}|T]) when V > 0 ->
    lists:duplicate(V, K) ++ do_elements(T);
do_elements([_|T]) ->
    do_elements(T).
