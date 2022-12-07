-module(chico_type_env).

-export([empty_type_env/0]).

-include("chico.hrl").

empty_type_env() -> #chico_type_env{vars = []}.
