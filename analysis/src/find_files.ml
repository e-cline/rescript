let if_debug debug name fn v = if debug then Log.log (name ^ ": " ^ fn v)
let ( /+ ) = Filename.concat
let bind f x = Option.bind x f

(* Returns a list of paths, relative to the provided `base` *)
let get_source_directories ~include_dev ~base_dir config =
  let rec handle_item current item =
    match item with
    | `List contents -> List.map (handle_item current) contents |> List.concat
    | `String text -> [current /+ text]
    | `Assoc _ -> (
      let dir =
        item |> Yojson_helpers.get "dir"
        |> bind Yojson_helpers.string_opt
        |> Option.value ~default:"Must specify directory"
      in
      let typ =
        if include_dev then "lib"
        else
          item |> Yojson_helpers.get "type"
          |> bind Yojson_helpers.string_opt
          |> Option.value ~default:"lib"
      in

      if typ = "dev" then []
      else
        match item |> Yojson_helpers.get "subdirs" with
        | None | Some (`Bool false) -> [current /+ dir]
        | Some (`Bool true | `String "qualified") ->
          Files.collect_dirs (base_dir /+ current /+ dir)
          |> List.filter (fun name -> name <> Filename.current_dir_name)
          |> List.map (Files.relpath base_dir)
        | Some item -> (current /+ dir) :: handle_item (current /+ dir) item)
    | _ -> failwith "Invalid subdirs entry"
  in
  match config |> Yojson_helpers.get "sources" with
  | None -> []
  | Some item -> handle_item "" item

(* The dirs of sources with "subdirs": "qualified", relative to the package
   root. Each directory below one is a module, so src/Ui/Button.res is
   Ui.Button, compiled as Button-Ui-<namespace>. *)
let get_qualified_source_roots config =
  let rec handle_item current item =
    match item with
    | `List contents -> List.concat_map (handle_item current) contents
    | `Assoc _ -> (
      match
        item |> Yojson_helpers.get "dir" |> bind Yojson_helpers.string_opt
      with
      | None -> []
      | Some dir -> (
        match item |> Yojson_helpers.get "subdirs" with
        | Some (`String "qualified") -> [current /+ dir]
        | Some ((`List _ | `Assoc _) as children) ->
          handle_item (current /+ dir) children
        | _ -> []))
    | _ -> []
  in
  match config |> Yojson_helpers.get "sources" with
  | None -> []
  | Some item -> handle_item "" item

let path_segments path =
  String.split_on_char '/' path
  |> List.concat_map (String.split_on_char '\\')
  |> List.filter (fun segment ->
      segment <> "" && segment <> Filename.current_dir_name)

(* The module names of the directories between the qualified source root
   that holds [relative_path] and the file, outermost first; [] for a file of
   any other source. *)
let qualifier_of_path ~qualified_roots relative_path =
  let rec strip prefix segments =
    match (prefix, segments) with
    | [], segments -> Some segments
    | first :: prefix, second :: segments when first = second ->
      strip prefix segments
    | _ :: _, _ -> None
  in
  let directories = path_segments (Filename.dirname relative_path) in
  qualified_roots
  |> List.find_map (fun root -> strip (path_segments root) directories)
  |> Option.fold ~none:[] ~some:(List.map String.capitalize_ascii)

(* The compilation unit of a qualified directory's namespace map, as the build
   names it: Controls-Ui-MyApp for [Ui; Controls] in namespace MyApp. *)
let directory_map_name ~namespace qualifier =
  String.concat "-" (List.rev qualifier @ Option.to_list namespace)

let is_compiled_file name =
  Filename.check_suffix name ".cmt" || Filename.check_suffix name ".cmti"

let is_implementation name =
  Filename.check_suffix name ".re"
  || Filename.check_suffix name ".res"
  || Filename.check_suffix name ".ml"

let is_interface name =
  Filename.check_suffix name ".rei"
  || Filename.check_suffix name ".resi"
  || Filename.check_suffix name ".mli"

let is_source_file name = is_implementation name || is_interface name

let compiled_name_space name =
  String.split_on_char '-' name
  |> List.map String.capitalize_ascii
  |> String.concat ""
  (* Remove underscores??? Whyyy bucklescript, whyyyy *)
  |> String.split_on_char '_'
  |> String.concat ""

let compiled_base_name ~namespace name =
  Filename.chop_extension name
  ^
  match namespace with
  | None -> ""
  | Some n -> "-" ^ compiled_name_space n

let get_name x =
  Filename.basename x |> Filename.chop_extension |> String.capitalize_ascii

let filter_duplicates cmts =
  (* Remove .cmt's that have .cmti's *)
  let intfs = Hashtbl.create 100 in
  cmts
  |> List.iter (fun path ->
      if
        Filename.check_suffix path ".rei"
        || Filename.check_suffix path ".mli"
        || Filename.check_suffix path ".cmti"
      then Hashtbl.add intfs (get_name path) true);
  cmts
  |> List.filter (fun path ->
      not
        ((Filename.check_suffix path ".re"
         || Filename.check_suffix path ".ml"
         || Filename.check_suffix path ".cmt")
        && Hashtbl.mem intfs (get_name path)))

let name_space_to_name n =
  n
  |> Str.split (Str.regexp "[-/@]")
  |> List.map String.capitalize_ascii
  |> String.concat ""

type namespace_config =
  | NamespaceDisabled
  | NamespaceFromPackageName
  | NamespaceExplicit of string

let get_namespace_config config =
  match config |> Yojson_helpers.get "namespace" with
  | None | Some (`Bool false) -> NamespaceDisabled
  | Some (`Bool true) -> NamespaceFromPackageName
  | Some (`String namespace) -> NamespaceExplicit namespace
  | Some _ -> NamespaceDisabled

let get_namespace config =
  match get_namespace_config config with
  | NamespaceDisabled -> None
  | NamespaceExplicit namespace -> Some (name_space_to_name namespace)
  | NamespaceFromPackageName ->
    let from_name =
      config |> Yojson_helpers.get "name" |> bind Yojson_helpers.string_opt
    in
    from_name |> Option.map name_space_to_name

module String_set = Set.Make (String)

let get_public config =
  let public = config |> Yojson_helpers.get "public" in
  match public with
  | None -> None
  | Some public -> (
    match public |> Yojson_helpers.to_list_opt with
    | None -> None
    | Some public ->
      Some
        (public
        |> List.filter_map Yojson_helpers.string_opt
        |> String_set.of_list))

let collect_files directory =
  let all_files = Files.read_directory directory in
  let compileds =
    all_files |> List.filter is_compiled_file |> filter_duplicates
  in
  let sources = all_files |> List.filter is_source_file |> filter_duplicates in
  compileds
  |> Utils.filter_map (fun path ->
      let mod_name = get_name path in
      let cmt = directory /+ path in
      let res_opt =
        Utils.find
          (fun name ->
            if get_name name = mod_name then Some (directory /+ name) else None)
          sources
      in
      match res_opt with
      | None -> None
      | Some res -> Some (mod_name, Shared_types.Impl {cmt; res}))

(* Dependency resolution uses the package graph recorded by the build system in
   .sourcedirs.json when available. If a package is not listed there, analysis
   falls back to walking up node_modules from the project root. *)
let read_sourcedirs_package_roots base =
  let source_dirs_file = base /+ "lib" /+ "bs" /+ ".sourcedirs.json" in
  let read_package_entry = function
    | `List [`String name; `String path] ->
      let path = if Filename.is_relative path then base /+ path else path in
      Some (name, path)
    | _ -> None
  in
  match Files.read_file source_dirs_file with
  | None -> []
  | Some text -> (
    match Yojson_helpers.from_string_opt text with
    | None -> []
    | Some json -> (
      match
        json |> Yojson_helpers.get "pkgs" |> bind Yojson_helpers.to_list_opt
      with
      | None -> []
      | Some packages -> packages |> List.filter_map read_package_entry))

let find_package_root ~base ~sourcedirs_package_roots name =
  match List.assoc_opt name sourcedirs_package_roots with
  | Some path when Files.exists path -> Some path
  | _ -> Module_resolution.resolve_node_module_path ~start_path:base name

(* returns a list of (absolute path to cmt(i), relative path from base to source file) *)
let find_project_files ~public ~namespace ~qualified_roots ~path
    ~source_directories ~lib_bs =
  let dirs =
    source_directories |> List.map (Filename.concat path) |> String_set.of_list
  in
  let files =
    (* Use max_depth to prevent infinite recursion where `rescript` depends on `@rescript/runtime`,
       but `@rescript/runtime` also has `rescript` as a dev dependency *)
    dirs |> String_set.elements
    |> List.map (fun name -> Files.collect ~max_depth:2 name is_source_file)
    |> List.concat |> String_set.of_list
  in
  dirs
  |> if_debug true "Source directories" (fun s ->
      s |> String_set.elements |> List.map Utils.dump_path |> String.concat " ");
  files
  |> if_debug true "Source files" (fun s ->
      s |> String_set.elements |> List.map Utils.dump_path |> String.concat " ");

  let qualifier file =
    qualifier_of_path ~qualified_roots (Files.relpath path file)
  in
  (* A file's module path, which tells apart same-named files of qualified
     directories. *)
  let module_path file = String.concat "." (qualifier file @ [get_name file]) in
  let interfaces = Hashtbl.create 100 in
  files
  |> String_set.iter (fun file ->
      if is_interface file then
        Hashtbl.replace interfaces (module_path file) file);

  let normals =
    files |> String_set.elements
    |> Utils.filter_map (fun file ->
        if is_implementation file then (
          let module_name = get_name file in
          let resi = Hashtbl.find_opt interfaces (module_path file) in
          Hashtbl.remove interfaces (module_path file);
          let qualifier = qualifier file in
          let base =
            match qualifier with
            | [] -> compiled_base_name ~namespace (Files.relpath path file)
            | _ :: _ ->
              Filename.chop_extension (Files.relpath path file)
              ^ "-"
              ^ directory_map_name
                  ~namespace:(Option.map compiled_name_space namespace)
                  qualifier
          in
          let unit_name =
            match (qualifier, namespace) with
            | [], None -> module_name
            | [], Some namespace -> module_name ^ "-" ^ namespace
            | _ :: _, _ ->
              module_name ^ "-" ^ directory_map_name ~namespace qualifier
          in
          let found paths = Some (module_name, unit_name, paths) in
          match resi with
          | Some resi ->
            let cmti = (lib_bs /+ base) ^ ".cmti" in
            let cmt = (lib_bs /+ base) ^ ".cmt" in
            if Files.exists cmti then
              if Files.exists cmt then
                (* Log.log("Intf and impl " ++ cmti ++ " " ++ cmt) *)
                found (Shared_types.IntfAndImpl {cmti; resi; cmt; res = file})
              else None
            else (
              (* Log.log("Just intf " ++ cmti) *)
              Log.log ("Bad source file (no cmt/cmti/cmi) " ^ (lib_bs /+ base));
              None)
          | None ->
            let cmt = (lib_bs /+ base) ^ ".cmt" in
            if Files.exists cmt then found (Shared_types.Impl {cmt; res = file})
            else (
              Log.log ("Bad source file (no cmt/cmi) " ^ (lib_bs /+ base));
              None))
        else None)
  in
  let result =
    normals
    |> List.filter_map (fun (original_name, name, paths) ->
        match public with
        | Some public ->
          if public |> String_set.mem original_name then Some (name, paths)
          else None
        | None -> Some (name, paths))
  in
  (* The namespace map of each qualified directory, which resolves the names
     of its modules and directories. *)
  let directory_maps =
    files |> String_set.elements
    |> List.concat_map (fun file ->
        let rec prefixes acc = function
          | [] -> []
          | directory :: rest ->
            let prefix = acc @ [directory] in
            prefix :: prefixes prefix rest
        in
        prefixes [] (qualifier file))
    |> List.sort_uniq compare
    |> List.map (fun qualifier ->
        let name = directory_map_name ~namespace qualifier in
        (name, Shared_types.Namespace {cmt = (lib_bs /+ name) ^ ".cmt"}))
  in
  match namespace with
  | None -> directory_maps @ result
  | Some namespace ->
    let module_name = name_space_to_name namespace in
    let cmt = (lib_bs /+ namespace) ^ ".cmt" in
    Log.log ("adding namespace " ^ namespace ^ " : " ^ module_name ^ " : " ^ cmt);
    ((module_name, Shared_types.Namespace {cmt}) :: directory_maps) @ result

let find_dependency_files base config =
  let deps =
    match
      ( config
        |> Yojson_helpers.get "dependencies"
        |> bind Yojson_helpers.to_list_opt,
        config
        |> Yojson_helpers.get "bs-dependencies"
        |> bind Yojson_helpers.to_list_opt )
    with
    | None, None -> []
    | Some deps, None | _, Some deps ->
      deps |> List.filter_map Yojson_helpers.string_opt
  in
  let dev_deps =
    match
      ( config
        |> Yojson_helpers.get "dev-dependencies"
        |> bind Yojson_helpers.to_list_opt,
        config
        |> Yojson_helpers.get "bs-dev-dependencies"
        |> bind Yojson_helpers.to_list_opt )
    with
    | None, None -> []
    | Some dev_deps, None | _, Some dev_deps ->
      dev_deps |> List.filter_map Yojson_helpers.string_opt
  in
  let deps = deps @ dev_deps in
  Log.log ("Dependencies: " ^ String.concat " " deps);
  let sourcedirs_package_roots = read_sourcedirs_package_roots base in
  let dep_files =
    deps
    |> List.map (fun name ->
        let result =
          bind
            (fun path ->
              let rescript_json_path = path /+ "rescript.json" in

              let parse_text text =
                match Yojson_helpers.from_string_opt text with
                | Some inner -> (
                  let namespace = get_namespace inner in
                  let source_directories =
                    get_source_directories ~include_dev:false ~base_dir:path
                      inner
                  in
                  match Build_system.get_lib_bs path with
                  | None -> None
                  | Some lib_bs ->
                    let compiled_directories =
                      source_directories |> List.map (Filename.concat lib_bs)
                    in
                    let compiled_directories =
                      match namespace with
                      | None -> compiled_directories
                      | Some _ -> lib_bs :: compiled_directories
                    in
                    let project_files =
                      find_project_files ~public:(get_public inner) ~namespace
                        ~qualified_roots:(get_qualified_source_roots inner)
                        ~path ~source_directories ~lib_bs
                    in
                    Some (compiled_directories, project_files))
                | None -> None
              in

              match Files.read_file rescript_json_path with
              | Some text -> parse_text text
              | None -> None)
            (find_package_root ~base ~sourcedirs_package_roots name)
        in

        match result with
        | Some (files, directories) -> (files, directories)
        | None ->
          Log.log ("Skipping nonexistent dependency: " ^ name);
          ([], []))
  in
  match Build_system.get_stdlib base with
  | None -> None
  | Some stdlib_directory ->
    let compiled_directories, project_files =
      let files, directories = List.split dep_files in
      (List.concat files, List.concat directories)
    in
    let all_files = project_files @ collect_files stdlib_directory in
    Some (compiled_directories, all_files)
