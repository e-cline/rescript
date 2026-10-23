(** Namespace maps are graph nodes because adding or removing a namespace
    member can invalidate consumers even when no ordinary source dependency
    name changes. The graph therefore contains both source modules and these
    synthetic nodes. *)

type cycle_info = {
  cycle: string list;
  blocked: string list;
  nodes_by_key: (string, cycle_node) Hashtbl.t;
}

and cycle_node = {
  key: string;
  package_root: string;
  source_path: string option;
  display_name: string;
}

type module_node = {
  key: string;
  package_name: string;
  package_root: string;
  source_path: string;
  namespace: Config.namespace;
  qualifier: string list;
  visible_packages: (string, unit) Hashtbl.t;
  mutable raw_dependencies: string list;
}

type namespace_map = {
  key: string;
  compiler_name: string;
  namespace: string;
      (** The package namespace, [""] for a package without one. *)
  qualifier: string list;
      (** [[]] for the package namespace, else the qualified directory. *)
  package_name: string;
  package_root: string;
  members: string list;
}

(** What a module path names in a package: a source module's unit, or the
    namespace map of a qualified directory. *)
type module_path_target =
  | Module of string
  | Directory of {package_root: string; qualifier: string list; key: string}

val namespace_map_key : string -> string
val directory_map_key : string -> string list -> string
val module_path_key : string -> string list -> string

val validate_visible_namespaces :
  root_config:Config.t -> Package_plan.t list -> unit

val resolved_dependencies :
  find_module:(string -> module_node option) ->
  find_namespace_maps:(string -> namespace_map list option) ->
  find_module_path:
    (package_root:string -> string list -> module_path_target option) ->
  module_node ->
  string list

val raw_dependencies :
  build_dir:string ->
  parse_failed:(string -> bool) ->
  Source.module_ ->
  string list
(** The sorted module names that a module's ASTs reference, skipping the
    source paths whose parse failed. *)

type initialized = {
  nodes: module_node list;
  namespace_maps: namespace_map list;
  module_paths: (string, module_path_target) Hashtbl.t;
      (** Keyed by [module_path_key]. *)
  build_state: Build_state.t;
  use_existing_ast_paths: string list;
}

val initialize :
  root_config:Config.t ->
  package_plans:Package_plan.t list ->
  compile_assets:Compile_assets.t ->
  failed_parse_paths:(string, unit) Hashtbl.t ->
  initialized

val find_cycle :
  module_node list -> namespace_map list -> Build_state.t -> cycle_info option
