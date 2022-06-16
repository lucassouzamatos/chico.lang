-module(chico_type_checker).

-export([check/1]).

check([]) -> [];
check([T]) -> check(T);
check([T|Rest]) -> check(T) ++ check(Rest);

check({apply, {{operator,_,_},{integer,_,_},{integer,_,_}}}) -> ok;
check({apply, {{operator,_,_},{integer,_,_}, _}}) -> throw("apply operator must be have two numbers").
