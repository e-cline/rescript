open OUnit2

(* The modules of package "app", namespace MyApp, with a qualified src:
   src/App.res, src/Ui/Button.res, src/Ui/Input.res,
   src/Ui/Controls/Button.res and src/Admin/Button.res, and of package "kit",
   namespace Kit, with src/Ui/Button.res. *)
let module_paths =
  let table = Hashtbl.create 16 in
  let add package_root path target =
    Hashtbl.replace table
      (Module_graph.module_path_key package_root path)
      target
  in
  let directory package_root qualifier =
    add package_root qualifier
      (Module_graph.Directory
         {
           package_root;
           qualifier;
           key = Module_graph.directory_map_key package_root qualifier;
         })
  in
  add "app" ["App"] (Module_graph.Module "App-MyApp");
  add "app" ["Ui"; "Button"] (Module_graph.Module "Button-Ui-MyApp");
  add "app" ["Ui"; "Input"] (Module_graph.Module "Input-Ui-MyApp");
  add "app"
    ["Ui"; "Controls"; "Button"]
    (Module_graph.Module "Button-Controls-Ui-MyApp");
  add "app" ["Admin"; "Button"] (Module_graph.Module "Button-Admin-MyApp");
  directory "app" ["Ui"];
  directory "app" ["Ui"; "Controls"];
  directory "app" ["Admin"];
  add "kit" ["Ui"; "Button"] (Module_graph.Module "Button-Ui-Kit");
  directory "kit" ["Ui"];
  table

let kit_map : Module_graph.namespace_map =
  {
    key = Module_graph.namespace_map_key "kit";
    compiler_name = "Kit";
    namespace = "Kit";
    qualifier = [];
    package_name = "kit";
    package_root = "kit";
    members = [];
  }

let dependencies ~qualifier ~name raw_dependencies =
  let visible_packages = Hashtbl.create 2 in
  Hashtbl.replace visible_packages "app" ();
  Hashtbl.replace visible_packages "kit" ();
  let node : Module_graph.module_node =
    {
      key = name ^ "-" ^ String.concat "-" (List.rev qualifier @ ["MyApp"]);
      package_name = "app";
      package_root = "app";
      source_path = "src/" ^ String.concat "/" (qualifier @ [name]) ^ ".res";
      namespace = Config.Namespace "MyApp";
      qualifier;
      visible_packages;
      raw_dependencies;
    }
  in
  Module_graph.resolved_dependencies
    ~find_module:(fun _ -> None)
    ~find_namespace_maps:(function
      | "Kit" -> Some [kit_map]
      | _ -> None)
    ~find_module_path:(fun ~package_root path ->
      Hashtbl.find_opt module_paths
        (Module_graph.module_path_key package_root path))
    node

let check_dependencies ~qualifier ~name raw expected message =
  assert_equal ~msg:message ~printer:(String.concat ", ")
    (List.sort String.compare expected)
    (dependencies ~qualifier ~name raw)

let tests =
  "module_graph_tests" >:: fun _context ->
  check_dependencies ~qualifier:[] ~name:"App"
    ["Ui"; "Ui.Button"; "Ui.Button.Icon"; "Ui.Controls"; "Ui.Controls.Button"]
    ["Button-Ui-MyApp"; "Button-Controls-Ui-MyApp"]
    "a qualified path depends on the module it names, not its directories, and \
     a module inside a file on that file";
  check_dependencies ~qualifier:["Ui"] ~name:"Input" ["Button"]
    ["Button-Ui-MyApp"] "a plain name is the module of the same directory";
  check_dependencies ~qualifier:["Ui"; "Controls"] ~name:"Button"
    ["Input"; "App"]
    ["Input-Ui-MyApp"; "App-MyApp"]
    "a plain name is found in the nearest directory above that has it";
  check_dependencies ~qualifier:["Ui"; "Controls"] ~name:"Button" ["Button"] []
    "a module's own name in its directory is itself";
  check_dependencies ~qualifier:["Admin"] ~name:"Button"
    ["Ui"; "Ui.Button"; "MyApp"; "MyApp.Ui"; "MyApp.Ui.Input"]
    ["Button-Ui-MyApp"; "Input-Ui-MyApp"]
    "a path from the package namespace resolves from its top level";
  check_dependencies ~qualifier:["Ui"; "Controls"] ~name:"Button" ["Ui"] []
    "naming one of the module's own directories adds no dependency";
  check_dependencies ~qualifier:["Ui"] ~name:"Input" ["Admin"; "Button"]
    [
      "Button-Ui-MyApp";
      "Button-Admin-MyApp";
      Module_graph.directory_map_key "app" ["Admin"];
    ]
    "after open Admin, a plain name may be in Admin, and the module waits for \
     all of it";
  check_dependencies ~qualifier:[] ~name:"App"
    ["Kit"; "Kit.Ui"; "Kit.Ui.Button"]
    ["Button-Ui-Kit"; Module_graph.namespace_map_key "kit"]
    "a path into another package's namespace names its qualified module, \
     beside the package's map, as for a flat package"
