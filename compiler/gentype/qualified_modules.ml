open Gentype_common

(* In a source with "subdirs": "qualified", src/Ui/Button.res is module
   Ui.Button, compiled as Button-Ui-MyApp in namespace MyApp. Types name it
   through the namespace map of the package or of a directory around the
   module using it, MyApp.Ui.Button or Ui-MyApp.Button, which genType resolves
   to the file here. *)

let index_key = Request_local.new_key (fun () -> Hashtbl.create 1)

(* The modules of the qualified sources by module path, [Ui.Button] mapped to
   src/Ui/Button: the source's path relative to the project root, without its
   extension. Top-level modules are left to genType's usual resolution. *)
let modules ~(config : Config.t) =
  let key =
    String.concat "\000" (config.project_root :: config.qualified_sources)
  in
  let cache = Domain.DLS.get index_key in
  match Hashtbl.find_opt cache key with
  | Some table -> table
  | None ->
    let table = Hashtbl.create 16 in
    let is_directory path =
      try Sys.is_directory path with Sys_error _ -> false
    in
    let rec walk ~relative ~qualifier =
      match Sys.readdir (Filename.concat config.project_root relative) with
      | exception Sys_error _ -> ()
      | entries ->
        Array.sort compare entries;
        entries
        |> Array.iter (fun entry ->
            let path = Filename.concat relative entry in
            if is_directory (Filename.concat config.project_root path) then
              walk ~relative:path
                ~qualifier:(qualifier @ [String.capitalize_ascii entry])
            else if
              qualifier <> []
              && (Filename.check_suffix entry ".res"
                 || Filename.check_suffix entry ".resi")
            then
              let name = Filename.remove_extension entry in
              Hashtbl.replace table
                (String.concat "." (qualifier @ [String.capitalize_ascii name]))
                (Filename.concat relative name))
    in
    List.iter
      (fun root -> walk ~relative:root ~qualifier:[])
      config.qualified_sources;
    Hashtbl.replace cache key table;
    table

(* Every directory holding a module, longest first. *)
let directories ~config =
  Hashtbl.fold
    (fun module_path _ acc ->
      let rec prefixes acc = function
        | [] | [_] -> acc
        | directory :: rest ->
          let prefix =
            match acc with
            | [] -> [directory]
            | outer :: _ -> outer @ [directory]
          in
          prefixes (prefix :: acc) rest
      in
      prefixes [] (String.split_on_char '.' module_path) @ acc)
    (modules ~config) []
  |> List.sort_uniq (fun first second ->
      match compare (List.length second) (List.length first) with
      | 0 -> compare first second
      | order -> order)

let map_name ~package_namespace qualifier =
  String.concat "-" (List.rev qualifier @ Option.to_list package_namespace)

let segments path =
  String.split_on_char '/' path
  |> List.concat_map (String.split_on_char '\\')
  |> List.filter (fun segment -> segment <> "" && segment <> ".")

(* The directories of the module at [source_file] below the qualified source
   that holds it, [["Ui"]] for src/Ui/Button.res, as discovery names them. *)
let qualifier_of_source ~(config : Config.t) source_file =
  let rec strip prefix path =
    match (prefix, path) with
    | [], path -> Some path
    | first :: prefix, second :: path when first = second -> strip prefix path
    | _ :: _, _ -> None
  in
  let directories =
    let path = segments (Filename.dirname source_file) in
    if Filename.is_relative source_file then Some path
    else strip (segments config.project_root) path
  in
  match directories with
  | None -> []
  | Some directories ->
    config.qualified_sources
    |> List.find_map (fun root -> strip (segments root) directories)
    |> Option.fold ~none:[] ~some:(List.map String.capitalize_ascii)

(* genType reads a module's namespace from its compilation unit, which for a
   module of a qualified directory is the directory's map, Controls-Ui-MyApp.
   The package namespace is what follows the module's own directories. *)
let package_namespace ~(config : Config.t) =
  match (config.namespace, config.source_qualifier) with
  | None, _ -> None
  | Some namespace, [] -> Some namespace
  | Some namespace, qualifier ->
    let directories = map_name ~package_namespace:None qualifier in
    if namespace = directories then None
    else if String.starts_with ~prefix:(directories ^ "-") namespace then
      Some
        (String.sub namespace
           (String.length directories + 1)
           (String.length namespace - String.length directories - 1))
    else Some namespace

(* The qualified module that a path of module names starts with, and the rest
   of the path: [MyApp; Ui; Button; t] is [Ui.Button] and [t]. *)
let qualify ~config path =
  let modules = modules ~config in
  if Hashtbl.length modules = 0 then None
  else
    let package_namespace = package_namespace ~config in
    let path =
      match path with
      | [] -> []
      | head :: rest -> (
        if Some head = package_namespace then rest
        else
          match
            directories ~config
            |> List.find_opt (fun qualifier ->
                map_name ~package_namespace qualifier = head)
          with
          | Some qualifier -> qualifier @ rest
          | None -> path)
    in
    let rec longest prefix rest best =
      match rest with
      | [] -> best
      | name :: rest ->
        let prefix = prefix @ [name] in
        let module_path = String.concat "." prefix in
        let best =
          if Hashtbl.mem modules module_path then Some (module_path, rest)
          else best
        in
        longest prefix rest best
    in
    longest [] path None

let source ~config module_path = Hashtbl.find_opt (modules ~config) module_path

(* The compilation unit of the qualified module whose source, relative to the
   project root and without its extension, is [path]. *)
let unit_of_source ~config path =
  Hashtbl.fold
    (fun module_path source acc ->
      if source = path then
        match List.rev (String.split_on_char '.' module_path) with
        | _name :: rev_qualifier ->
          Some
            (Filename.basename source ^ "-"
            ^ map_name
                ~package_namespace:(package_namespace ~config)
                (List.rev rev_qualifier))
        | [] -> acc
      else acc)
    (modules ~config) None
