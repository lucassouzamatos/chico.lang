-module(chico_bootstrap).

-export([start/0]).

start() -> [compile_lib_file(Name) || Name <- [
  "lib/pair.chico",
  "lib/recur.chico",
  "lib/list.chico"
]].

compile_lib_file(Filename) ->
  { ok, Cwd } = file:get_cwd(),
  
  File = filename:join(Cwd, Filename),

  case file:read_file(File) of
    {ok, Source} -> 
      { Module, Bin, _ } = chico_compiler:compile_file(Source, File),
      file:write_file(filename:join("/usr/chicolang/bin", Module ++ ".beam"), Bin);
    _ -> error
  end.