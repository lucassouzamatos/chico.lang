## Backend

Ar-lang is interpreted and translated to backend erlang. Some examples and notes about this translation:

### Calc operator

```
apply + 2 2
```

This is converted following the example:

```erlang
{ ok, FTS, _ } = erl_scan:string("2 + 2.").
erl_parse:parse_exprs(FTS).

% prints: {ok,[{op,1,'+',{integer,1,2},{integer,1,2}}]}
```
