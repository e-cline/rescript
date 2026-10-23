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
  qualifier: string list;
  package_name: string;
  package_root: string;
  members: string list;
}

type module_path_target =
  | Module of string
  | Directory of {package_root: string; qualifier: string list; key: string}

let namespace_map_key package_root = "\000namespace:" ^ package_root

let directory_map_key package_root qualifier =
  namespace_map_key package_root ^ "\000" ^ String.concat "." qualifier

let module_path_key package_root path =
  package_root ^ "\000" ^ String.concat "." path

let dependency_head dependency =
  match String.split_on_char '.' dependency with
  | head :: _ -> head
  | [] -> dependency

let compiler_namespace (config : Config.t) =
  Config.namespace_compiler_name config.namespace

let validate_visible_namespaces ~(root_config : Config.t)
    (package_plans : Package_plan.t list) =
  let by_root = Hashtbl.create (List.length package_plans) in
  List.iter
    (fun (package : Package_plan.t) ->
      Hashtbl.replace by_root package.root package)
    package_plans;
  package_plans
  |> List.sort (fun first second ->
      String.compare first.Package_plan.root second.root)
  |> List.iter (fun consumer ->
      let visible =
        consumer
        :: (consumer.Package_plan.dependencies
           |> List.filter_map (fun dependency ->
               Hashtbl.find_opt by_root dependency.Package_plan.directory))
      in
      let namespaces = Hashtbl.create (List.length visible) in
      visible
      |> List.sort (fun first second ->
          String.compare first.Package_plan.root second.root)
      |> List.iter (fun package ->
          compiler_namespace package.Package_plan.compile_config
          |> Option.iter (fun namespace ->
              match Hashtbl.find_opt namespaces namespace with
              | None -> Hashtbl.add namespaces namespace package
              | Some previous when previous.Package_plan.root = package.root ->
                ()
              | Some previous ->
                let display package =
                  Printf.sprintf "%s (%s)" package.Package_plan.config.name
                    (Project_context.display_path ~root:root_config.root
                       package.root)
                in
                raise
                  (Project_context.Error
                     (Printf.sprintf
                        "Could not initialize build: Namespace %s is provided \
                         by both %s and %s while building %s. Give the \
                         packages distinct namespaces."
                        namespace (display previous) (display package)
                        (display consumer))))))

let resolve_dependency ~find_module ~find_namespace_maps (node : module_node)
    dependency =
  let raw_name = dependency_head dependency in
  let local_name =
    match
      (Config.namespace_name node.namespace, String.split_on_char '.' dependency)
    with
    | Some namespace, first :: second :: _ when first = namespace -> second
    | _ -> raw_name
  in
  let local_key = Config.namespaced_module_name node.namespace local_name in
  let is_visible (dependency_node : module_node) =
    Hashtbl.mem node.visible_packages dependency_node.package_name
  in
  match find_module local_key with
  | Some dependency_node when is_visible dependency_node -> [local_key]
  | _ when Config.namespace_name node.namespace = Some raw_name -> []
  | _ -> (
    match find_module raw_name with
    | Some dependency_node when is_visible dependency_node -> [raw_name]
    | _ -> (
      let explicit_namespaced_module =
        match String.split_on_char '.' dependency with
        | namespace :: module_name :: _ ->
          [module_name ^ "-" ^ namespace; module_name ^ "-@" ^ namespace]
          |> List.find_opt (fun key ->
              match find_module key with
              | Some dependency_node
                when Config.namespace_name dependency_node.namespace
                     = Some namespace
                     && is_visible dependency_node ->
                true
              | Some _ | None -> false)
        | _ -> None
      in
      match explicit_namespaced_module with
      | Some key -> [key]
      | None ->
        find_namespace_maps raw_name
        |> Option.value ~default:[]
        |> List.filter_map (fun (namespace_map : namespace_map) ->
            if Hashtbl.mem node.visible_packages namespace_map.package_name then
              Some namespace_map.key
            else None)))

let rec is_prefix prefix path =
  match (prefix, path) with
  | [], _ -> true
  | first :: prefix, second :: path -> first = second && is_prefix prefix path
  | _ :: _, [] -> false

(* Resolves a dependency by the module paths of the packages it can see, as
   the compiler does through the namespace maps the module opens: in the
   nearest of its directories that binds the path's first name, else in the
   package namespace it names. [Some None] is a path into a source module,
   such as [Ui.Button.Icon], whose module the header's shorter prefix names.
   [None] leaves it to [resolve_dependency]. *)
let resolve_module_path ~find_module_path ~find_namespace_maps
    (node : module_node) path =
  let find = find_module_path ~package_root:node.package_root in
  let rec scopes qualifier =
    qualifier
    ::
    (match List.rev qualifier with
    | [] -> []
    | _ :: outer -> scopes (List.rev outer))
  in
  match path with
  | [] -> None
  | head :: rest -> (
    match
      List.find_opt
        (fun scope -> Option.is_some (find (scope @ [head])))
        (scopes node.qualifier)
    with
    | Some scope -> Some (find (scope @ path))
    | None when Config.namespace_name node.namespace = Some head ->
      Some (if rest = [] then None else find rest)
    | None -> (
      match rest with
      | [] -> None
      | _ :: _ ->
        find_namespace_maps head |> Option.value ~default:[]
        |> List.find_map (fun (namespace_map : namespace_map) ->
            if Hashtbl.mem node.visible_packages namespace_map.package_name then
              find_module_path ~package_root:namespace_map.package_root rest
              |> Option.map Option.some
            else None)))

let resolved_dependencies ~find_module ~find_namespace_maps ~find_module_path
    (node : module_node) =
  let resolved =
    node.raw_dependencies
    |> List.map (fun dependency ->
        ( dependency,
          resolve_module_path ~find_module_path ~find_namespace_maps node
            (String.split_on_char '.' dependency) ))
  in
  let names_module dependency =
    List.exists
      (fun (other, target) ->
        String.starts_with ~prefix:(dependency ^ ".") other
        &&
        match target with
        | Some (Some (Module _)) -> true
        | Some (Some (Directory _)) | Some None | None -> false)
      resolved
  in
  (* A directory named on its own, as by [open Ui], makes its modules what a
     plain name may refer to. Unless it is one of the module's own
     directories, whose maps it depends on anyway, or a path through it names
     the module used, the module waits for all of the directory, since an
     alias such as [module U = Ui] hides which one it uses. *)
  let opened =
    List.filter_map
      (fun (_, target) ->
        match target with
        | Some (Some (Directory {package_root; qualifier; key = _}))
          when package_root = node.package_root ->
          Some qualifier
        | Some (Some (Directory _ | Module _)) | Some None | None -> None)
      resolved
  in
  let parsed =
    resolved
    |> List.concat_map (fun (dependency, target) ->
        match target with
        | Some (Some (Module key)) -> [key]
        | Some (Some (Directory {package_root; qualifier; key})) ->
          if
            package_root = node.package_root
            && is_prefix qualifier node.qualifier
            || names_module dependency
          then []
          else [key]
        | Some None -> []
        | None ->
          resolve_dependency ~find_module ~find_namespace_maps node dependency)
  in
  let in_opened_directories =
    node.raw_dependencies
    |> List.filter (fun dependency -> not (String.contains dependency '.'))
    |> List.concat_map (fun name ->
        List.filter_map
          (fun qualifier ->
            match
              find_module_path ~package_root:node.package_root
                (qualifier @ [name])
            with
            | Some (Module key) -> Some key
            | Some (Directory _) | None -> None)
          opened)
  in
  let parsed = parsed @ in_opened_directories in
  let implicit_namespace_entry =
    match node.namespace with
    | Config.Namespace_with_entry {name = namespace; entry}
      when node.qualifier = [] && Source.module_name node.source_path = entry ->
      find_namespace_maps namespace
      |> Option.value ~default:[]
      |> List.find_map (fun (namespace_map : namespace_map) ->
          if namespace_map.package_root = node.package_root then
            Some namespace_map.key
          else None)
      |> Option.to_list
    | Config.Namespace_with_entry _ | Config.Namespace _ | Config.No_namespace
      ->
      []
  in
  parsed @ implicit_namespace_entry
  |> List.filter (fun dependency -> dependency <> node.key)
  |> List.sort_uniq String.compare

let raw_dependencies ~build_dir ~parse_failed (module_ : Source.module_) =
  let dependencies path =
    if parse_failed path then []
    else Compiler_process.ast_dependencies ~build_dir (Source.ast_path path)
  in
  List.sort_uniq String.compare
    (dependencies module_.implementation
    @
    match module_.interface with
    | None -> []
    | Some path -> dependencies path)

type initialized = {
  nodes: module_node list;
  namespace_maps: namespace_map list;
  module_paths: (string, module_path_target) Hashtbl.t;
  build_state: Build_state.t;
  use_existing_ast_paths: string list;
}

let initialize ~(root_config : Config.t) ~package_plans ~compile_assets
    ~failed_parse_paths =
  let nodes = ref [] in
  let use_existing_ast_paths = ref [] in
  List.iter
    (fun (package : Package_plan.t) ->
      let visible_packages =
        Hashtbl.create (List.length package.dependencies + 1)
      in
      Hashtbl.replace visible_packages package.name ();
      List.iter
        (fun (dependency : Package_plan.dependency) ->
          Hashtbl.replace visible_packages dependency.declaration.name ())
        package.dependencies;
      List.iter
        (fun module_ ->
          let raw_dependencies =
            raw_dependencies ~build_dir:package.build_dir
              ~parse_failed:(fun path ->
                Hashtbl.mem failed_parse_paths
                  (Filename.concat package.root path))
              module_
          in
          let compiler_base =
            Source.compiler_basename package.compile_config module_
          in
          if Option.is_none (Compile_assets.cmt compile_assets compiler_base)
          then
            use_existing_ast_paths :=
              Filename.concat package.root module_.Source.implementation
              :: !use_existing_ast_paths;
          nodes :=
            {
              key = compiler_base;
              package_name = package.name;
              package_root = package.root;
              source_path = module_.Source.implementation;
              namespace = package.compile_config.namespace;
              qualifier = module_.Source.qualifier;
              visible_packages;
              raw_dependencies;
            }
            :: !nodes)
        package.modules)
    package_plans;
  let nodes =
    List.sort
      (fun (first : module_node) second -> String.compare first.key second.key)
      !nodes
  in
  let by_key : (string, module_node) Hashtbl.t =
    Hashtbl.create (List.length nodes)
  in
  List.iter
    (fun (node : module_node) ->
      match Hashtbl.find_opt by_key node.key with
      | None -> Hashtbl.add by_key node.key node
      | Some previous ->
        raise
          (Source.duplicate_error ~display_root:root_config.root "" node.key
             (Filename.concat previous.package_root previous.source_path)
             (Filename.concat node.package_root node.source_path)))
    nodes;
  let module_paths = Hashtbl.create (List.length nodes) in
  let namespace_maps =
    package_plans
    |> List.concat_map (fun (package : Package_plan.t) ->
        let config = package.Package_plan.compile_config in
        let entry = Config.namespace_entry config.namespace in
        let directories = Source.directories package.modules in
        List.iter
          (fun module_ ->
            Hashtbl.replace module_paths
              (module_path_key package.root
                 (module_.Source.qualifier @ [module_.Source.name]))
              (Module (Source.compiler_basename config module_)))
          package.modules;
        List.iter
          (fun qualifier ->
            Hashtbl.replace module_paths
              (module_path_key package.root qualifier)
              (Directory
                 {
                   package_root = package.root;
                   qualifier;
                   key = directory_map_key package.root qualifier;
                 }))
          directories;
        (* A map depends on what it binds, so a module that needs a whole
           namespace waits for every module in it. *)
        let members qualifier =
          Source.map_members ~entry package.modules qualifier
          |> List.filter_map (fun name ->
              match
                Hashtbl.find_opt module_paths
                  (module_path_key package.root (qualifier @ [name]))
              with
              | Some (Module key) -> Some key
              | Some (Directory {key}) -> Some key
              | None -> None)
          |> List.sort_uniq String.compare
        in
        let map ~key ~compiler_name ~qualifier =
          {
            key;
            compiler_name;
            namespace =
              Option.value (Config.namespace_name config.namespace) ~default:"";
            qualifier;
            package_name = package.name;
            package_root = package.root;
            members = members qualifier;
          }
        in
        let package_map =
          Config.namespace_compiler_name config.namespace
          |> Option.map (fun compiler_name ->
              map
                ~key:(namespace_map_key package.root)
                ~compiler_name ~qualifier:[])
          |> Option.to_list
        in
        package_map
        @ List.map
            (fun qualifier ->
              map
                ~key:(directory_map_key package.root qualifier)
                ~compiler_name:(Source.directory_map_name config qualifier)
                ~qualifier)
            directories)
  in
  let namespace_maps_by_name = Hashtbl.create (List.length namespace_maps) in
  List.iter
    (fun (namespace_map : namespace_map) ->
      if namespace_map.qualifier = [] then
        let existing =
          Hashtbl.find_opt namespace_maps_by_name namespace_map.namespace
          |> Option.value ~default:[]
        in
        Hashtbl.replace namespace_maps_by_name namespace_map.namespace
          (namespace_map :: existing))
    namespace_maps;
  let find_module_path ~package_root path =
    Hashtbl.find_opt module_paths (module_path_key package_root path)
  in
  let source_graph_nodes =
    List.map
      (fun (node : module_node) ->
        ( node,
          resolved_dependencies ~find_module:(Hashtbl.find_opt by_key)
            ~find_namespace_maps:(Hashtbl.find_opt namespace_maps_by_name)
            ~find_module_path node ))
      nodes
  in
  let build_state =
    Build_state.create
      (List.length source_graph_nodes + List.length namespace_maps)
  in
  let modified = Option.map (fun entry -> entry.Compile_assets.modified) in
  List.iter
    (fun ((node : module_node), _) ->
      Build_state.add build_state ~key:node.key ~kind:Build_state.Source_module
        ~last_compiled_cmi:
          (Compile_assets.cmi compile_assets node.key |> modified)
        ~last_compiled_cmt:
          (Compile_assets.cmt compile_assets node.key |> modified))
    source_graph_nodes;
  List.iter
    (fun (namespace_map : namespace_map) ->
      Build_state.add build_state ~key:namespace_map.key
        ~kind:Build_state.Namespace_map
        ~last_compiled_cmi:
          (Compile_assets.cmi compile_assets namespace_map.compiler_name
          |> modified)
        ~last_compiled_cmt:
          (Compile_assets.cmt compile_assets namespace_map.compiler_name
          |> modified))
    namespace_maps;
  List.iter
    (fun ((node : module_node), dependencies) ->
      Build_state.set_dependencies build_state ~key:node.key dependencies)
    source_graph_nodes;
  List.iter
    (fun (namespace_map : namespace_map) ->
      Build_state.set_dependencies build_state ~key:namespace_map.key
        namespace_map.members)
    namespace_maps;
  {
    nodes;
    namespace_maps;
    module_paths;
    build_state;
    use_existing_ast_paths = !use_existing_ast_paths;
  }

let find_cycle modules namespace_maps build_state =
  let nodes_by_key =
    Hashtbl.create (List.length modules + List.length namespace_maps)
  in
  List.iter
    (fun (node : module_node) ->
      let key = node.key in
      let module_name = Source.module_name node.source_path in
      let module_path = String.concat "." (node.qualifier @ [module_name]) in
      let display_name =
        match node.namespace with
        | Config.Namespace_with_entry {name = namespace; entry}
          when node.qualifier <> [] || entry <> module_name ->
          namespace ^ "." ^ module_path
        | Config.Namespace namespace -> namespace ^ "." ^ module_path
        | Config.Namespace_with_entry _ | Config.No_namespace -> module_path
      in
      Hashtbl.add nodes_by_key key
        {
          key;
          package_root = node.package_root;
          source_path = Some node.source_path;
          display_name;
        })
    modules;
  List.iter
    (fun (namespace_map : namespace_map) ->
      let key = namespace_map.key in
      Hashtbl.add nodes_by_key key
        {
          key;
          package_root = namespace_map.package_root;
          source_path = None;
          display_name =
            (match namespace_map.qualifier with
            | [] -> namespace_map.compiler_name
            | qualifier ->
              String.concat "."
                ((if namespace_map.namespace = "" then []
                  else [namespace_map.namespace])
                @ qualifier));
        })
    namespace_maps;
  let graph_nodes =
    Hashtbl.to_seq_values nodes_by_key
    |> List.of_seq
    |> List.sort (fun (first : cycle_node) second ->
        String.compare first.key second.key)
    |> List.map (fun (node : cycle_node) ->
        (node, (Build_state.find_exn build_state node.key).dependencies))
  in
  let name ((node : cycle_node), _) = node.key in
  let blocked_nodes = Graph.cycle_blocked_nodes graph_nodes ~name ~deps:snd in
  match blocked_nodes with
  | [] -> None
  | _ ->
    let cycle =
      match Graph.shortest_cycle blocked_nodes ~name ~deps:snd with
      | Some cycle -> cycle
      | None ->
        raise
          (Project_context.Error
             "cycle-blocked dependency graph contains no detectable cycle")
    in
    Some {cycle; blocked = List.map name blocked_nodes; nodes_by_key}
