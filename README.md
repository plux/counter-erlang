# counter

An Erlang library inspired by Python's [collections.Counter](https://docs.python.org/3/library/collections.html#collections.Counter)

## Build

```bash
$ rebar3 compile
```

## Examples

```erlang
> A = #{a => 2}.
#{a => 2}

> B = #{b => 5, d => 4}.
#{b => 5,d => 4}

> C = counter:count([a,b,c,b,b,a]).
#{a => 2,b => 3,c => 1}

> counter:count_with(fun(E) -> {E,5} end, [a,b,c]).
#{a => 5,b => 5,c => 5}

> counter:from_list([{a,1}, {a,1}, {b,1}, {b,2}, {c,1}]).
#{a => 2,b => 3,c => 1}

> counter:incr(C, a).
#{a => 3,b => 3,c => 1}

> counter:incr(C, a, 10).
#{a => 12,b => 3,c => 1}

> counter:add(C, C).
#{a => 4,b => 6,c => 2}

> counter:subtract(A,B).
#{a => 2,b => -5,d => -4}

> counter:dups(C).
#{a => 2,b => 3}

> counter:unique(C).
[c]

> counter:max(C).
{b,3}

> counter:most_common(C, 2).
[{b,3},{a,2}]

> counter:least_common(C, 2).
[{c,1},{a,2}]

> counter:min(C).
{c,1}

> counter:elements(C).
[a,a,b,b,b,c]

> counter:union(B, C).
#{a => 2,b => 5,c => 1,d => 4}

> counter:intersection(B, C).
#{b => 3}

> counter:only_positive(#{a => 5, b => -5, c => 0}).
#{a => 5}

> counter:sum(C).
6
```
