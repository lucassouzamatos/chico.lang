-module(chico_bootstrap).

-export([start/0]).

start() ->
  [compile_lib_file(Name) || Name <- ["lib/Recursive.chico", "lib/Request.chico", "lib/Std.chico"]].

compile_lib_file(Filename) ->
  {ok, Cwd} = file:get_cwd(),
  File = filename:join(Cwd, Filename),
  case file:read_file(File) of
    {ok, Source} ->
      {Module, Bin, _} = chico_compiler:compile_file(Source, File),
      Dest = filename:join("ebin", Module ++ ".beam"),
      file:write_file(Dest, Bin);

    _ -> error
  end.
