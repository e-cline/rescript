(** Discovery keeps filesystem files separate from compilable modules. Commands
    such as formatting can operate on orphan interfaces and duplicate basenames,
    while compilation uses [module_] after pairing and duplicate validation. *)

type module_ = {
  name: string;
  qualifier: string list;
      (** The module names of the directories between a qualified source and
          the module, outermost first: [["Ui"; "Controls"]] for
          src/Ui/Controls/Button.res in the qualified source src. Empty for a
          module of any other source. *)
  implementation: string;
  interface: string option;
  is_dev: bool;
}

type discovery = {
  modules: module_ list;
  source_mtimes: (string * float) list;
  inventory_files: string list;
  present_files: string list;
  gentype_dirs: string list;
}

exception Error of string

type source_kind = Implementation | Interface

val source_kind : string -> source_kind option
val module_name : string -> string

val without_current_dir_segments : string -> string
(** [path] without its "." segments: "./src/./A.res" is "src/A.res". *)

val duplicate_error :
  display_root:string -> string -> string -> string -> string -> exn

val resolve_active_features :
  Config.t -> string list -> (string, unit) Hashtbl.t

val active_sources :
  Config.t -> prod:bool -> features:string list option -> Config.source list

val discover_for_cleanup :
  on_missing:(string -> unit) -> Config.t -> prod:bool -> string list

val discover_files :
  on_missing:(string -> unit) ->
  Config.t ->
  prod:bool ->
  features:string list option ->
  string list

val discover_with_inventory :
  ?on_orphan:(string -> unit) ->
  on_missing:(string -> unit) ->
  ?display_root:string ->
  Config.t ->
  prod:bool ->
  features:string list option ->
  filter:Source_filter.t option ->
  discovery

val ast_path : string -> string

val qualifier_of_path : Config.t -> string -> string list
(** The [qualifier] that discovery gives the module at a source path. *)

val directory_map_name : Config.t -> string list -> string
(** The compilation unit of a qualified directory's namespace map:
    [Controls-Ui-MyApp] for [["Ui"; "Controls"]] in namespace MyApp. *)

val module_namespace : Config.t -> string list -> string option
(** The namespace that modules of a directory are compiled in, [None] for the
    top-level modules of a package without a namespace. *)

val compiler_basename : Config.t -> module_ -> string
(** The module's compilation unit: [Button-Ui-MyApp] for src/Ui/Button.res. *)

val compiler_asset_basename : Config.t -> string -> string

val published_basename : Config.t -> string -> string
(** The name of a source's copy, and its AST's, in lib/ocaml. *)

val dependency_names : Config.t -> string -> string list
(** The dependency names that a consumer of the module at a source path may
    record: its name for a module of a flat source, and for
    src/Ui/Button.res in namespace MyApp [Button], [MyApp], [MyApp.Ui],
    [MyApp.Ui.Button], [Ui] and [Ui.Button]. *)

val module_path : module_ -> string
(** [Ui.Controls.Button] for src/Ui/Controls/Button.res. *)

val directories : module_ list -> string list list
(** The qualifiers of every directory that holds a module. *)

val map_members :
  entry:string option -> module_ list -> string list -> string list
(** The names the namespace map of a directory binds, [[]] being the package
    namespace: its modules, without the namespace entry and exotic names, and
    the directories below it. *)
