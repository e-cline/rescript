val source_is_not_older_than_ast :
  Compile_assets.t ->
  config:Config.t ->
  root:string ->
  source_mtimes:(string, float) Hashtbl.t ->
  string ->
  bool
