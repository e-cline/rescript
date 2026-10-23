open OUnit2

let check condition message = assert_bool message condition

let write_file = Test_support.write_file

let names modules =
  List.map (fun (module_ : Source.module_) -> module_.name) modules

let discover config ?(prod = false) ?features ?filter () =
  let filter =
    Option.map
      (fun pattern ->
        match Source_filter.compile pattern with
        | Ok filter -> filter
        | Error message -> failwith message)
      filter
  in
  (Source.discover_with_inventory config ~on_missing:ignore ~prod ~features
     ~filter)
    .modules

let discover_with_inventory config ?(prod = false) ?features () =
  Source.discover_with_inventory config ~on_missing:ignore ~prod ~features
    ~filter:None

let discovery_tests _context =
  let namespace_modules =
    [
      Source.
        {
          name = "Entry";
          qualifier = [];
          implementation = "Entry.res";
          interface = None;
          is_dev = false;
        };
      Source.
        {
          name = "Member";
          qualifier = [];
          implementation = "Member.res";
          interface = None;
          is_dev = false;
        };
      Source.
        {
          name = "Member-with-dash";
          qualifier = [];
          implementation = "Member-with-dash.res";
          interface = None;
          is_dev = false;
        };
    ]
  in
  check
    (Source.map_members ~entry:(Some "Entry") namespace_modules [] = ["Member"])
    "namespace membership excludes the entry and exotic module names";
  let root = Filename.temp_file "rewatch-ocaml-sources-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  let root = Unix.realpath root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree root)
    (fun () ->
      write_file (Test_support.path root "src/Main.res") "let value = 1\n";
      write_file
        (Test_support.path root "src/nested/NotDiscovered.res")
        "let value = 1\n";
      write_file (Test_support.path root "test/Test.res") "let value = 1\n";
      write_file
        (Test_support.path root "test/nested/Nested.res")
        "let value = 1\n";
      write_file (Test_support.path root "native/Native.res") "let value = 1\n";
      write_file (Test_support.path root "native/Native.mjs") "export {}\n";
      let config_path = Filename.concat root "rescript.json" in
      write_file config_path
        {|{
          "name": "source-tests",
          "sources": [
            "src",
            {"dir": "test", "type": "dev", "subdirs": true},
            {"dir": "native", "feature": "native"}
          ]
        }|};
      let config = Config.load config_path in
      check
        (names (discover config ()) = ["Main"; "Native"; "Nested"; "Test"])
        "an unrestricted build includes shorthand, dev, recursive, and tagged \
         sources";
      check
        (names (discover config ~prod:true ()) = ["Main"; "Native"])
        "a production build excludes a recursive dev source";
      check
        (names (discover config ~features:["native"] ())
        = ["Main"; "Native"; "Nested"; "Test"])
        "an active leaf feature includes tagged and untagged sources";
      check
        (names (discover config ~features:["other"] ())
        = ["Main"; "Nested"; "Test"])
        "an inactive feature excludes only its tagged source";
      check
        (names (discover config ~filter:"^Nested\\.res$" ()) = ["Nested"])
        "source filters match file basenames";
      check
        (names (discover config ~filter:"test" ()) = [])
        "source filters do not match directory components";
      check
        (names (discover config ~filter:"Nested|Other" ()) = ["Nested"])
        "source filters support Rust-style alternation";
      check
        (names (discover config ~filter:"(?:Nested|Other)\\.res$" ())
        = ["Nested"])
        "source filters support non-capturing groups";
      check
        (names (discover config ~filter:"Nested\\d*\\.res$" ()) = ["Nested"])
        "source filters support shorthand classes and repetition";
      write_file config_path
        {|{
          "name": "cyclic-features",
          "sources": ["src", {"dir": "native", "feature": "a"}],
          "features": {"a": ["b"], "b": ["a"]}
        }|};
      let cyclic_config = Config.load config_path in
      check
        (names (discover cyclic_config ()) = ["Main"; "Native"])
        "an unrestricted feature selection does not traverse implication cycles";
      check
        (try
           ignore (discover cyclic_config ~features:["a"] ());
           false
         with Source.Error message ->
           Test_support.contains_text message "a -> b -> a")
        "a restricted feature selection rejects an implication cycle";
      let discovery = discover_with_inventory config ~features:["other"] () in
      check
        (List.mem
           (Test_support.path root "src/nested/NotDiscovered.res")
           discovery.inventory_files)
        "cleanup inventory descends through a non-recursive source";
      check
        (List.mem
           (Test_support.path root "native/Native.mjs")
           discovery.inventory_files)
        "cleanup inventory retains non-source files from an inactive feature";
      check
        (not (List.mem "NotDiscovered" (names discovery.modules)))
        "cleanup inventory does not make nested files into source modules";
      check
        (List.assoc (Filename.concat "src" "Main.res") discovery.source_mtimes
        = (Unix.stat (Test_support.path root "src/Main.res")).Unix.st_mtime)
        "source discovery retains the metadata used by freshness checks";
      write_file config_path
        {|{
          "name": "gentype-source-tests",
          "sources": [
            "src",
            {"dir": "test", "type": "dev", "subdirs": true},
            {"dir": "native", "feature": "native"}
          ],
          "gentypeconfig": {}
        }|};
      let discovery =
        Config.load config_path |> fun config ->
        discover_with_inventory config ~prod:true ~features:["other"] ()
      in
      check
        (discovery.gentype_dirs
        = ["src"; "test"; Filename.concat "test" "nested"])
        "GenType directories use active features but retain dev sources";
      write_file config_path
        {|{
          "name": "overlapping-source-tests",
          "sources": ["src", {"dir": "src", "subdirs": true}],
          "gentypeconfig": {}
        }|};
      let overlapping =
        Config.load config_path |> fun config ->
        discover_with_inventory config ()
      in
      check
        (names overlapping.modules = ["Main"; "NotDiscovered"])
        "a recursive declaration upgrades an earlier shallow traversal";
      check
        (overlapping.gentype_dirs = ["src"; Filename.concat "src" "nested"])
        "GenType traversal also upgrades overlapping source coverage";
      write_file (Test_support.path root "ignored/Nested.res") "let value = 1\n";
      write_file config_path
        {|{
          "name": "unsupported-ignored-dirs",
          "sources": {"dir": "ignored", "subdirs": true},
          "ignored-dirs": ["ignored"]
        }|};
      let config = Config.load config_path in
      check
        (names (discover config ()) = ["Nested"])
        "unsupported ignored-dirs does not suppress source discovery";
      write_file (Test_support.path root "case/lower.res") "let value = 1\n";
      write_file (Test_support.path root "case/Lower.resi") "let value: int\n";
      write_file config_path {|{"name":"interface-case","sources":["case"]}|};
      let config = Config.load config_path in
      let casing_rejected =
        try
          ignore (discover config ());
          false
        with Source.Error message ->
          Test_support.contains_text message
            "Could not initialize build: Implementation and interface have \
             different path names or different cases"
      in
      check casing_rejected
        "implementation and interface basename casing must match";
      write_file (Test_support.path root "case/Lower.res") "let value = 1\n";
      let lower = Unix.stat (Test_support.path root "case/lower.res") in
      let upper = Unix.stat (Test_support.path root "case/Lower.res") in
      (* Case-insensitive filesystems give both spellings the same directory
         entry, so they cannot represent the two inputs needed by this check. *)
      (if lower.st_dev <> upper.st_dev || lower.st_ino <> upper.st_ino then
         let duplicate_rejected =
           try
             ignore (discover config ());
             false
           with Source.Error message ->
             Test_support.contains_text message "Duplicate module name: Lower"
         in
         check duplicate_rejected
           "adding the exact implementation still exposes the \
            differently-cased duplicate");
      write_file (Test_support.path root "paths/a/Path.res") "let value = 1\n";
      write_file (Test_support.path root "paths/b/Path.resi") "let value: int\n";
      write_file config_path
        {|{"name":"interface-path","sources":{"dir":"paths","subdirs":true}}|};
      let config = Config.load config_path in
      let path_rejected =
        try
          ignore (discover config ());
          false
        with Source.Error message ->
          Test_support.contains_text message
            "different path names or different cases"
      in
      check path_rejected
        "an interface cannot attach to a same-named implementation in another \
         directory";
      if not Sys.win32 then (
        write_file
          (Test_support.path root "linked-target/Linked.res")
          "let value = 1\n";
        Unix.symlink
          (Filename.concat root "linked-target")
          (Filename.concat root "linked-source");
        write_file config_path
          {|{"name":"linked-source","sources":["linked-source"]}|};
        let discovery =
          Config.load config_path |> fun config ->
          discover_with_inventory config ()
        in
        check
          (names discovery.modules = ["Linked"])
          "source discovery follows a configured directory symlink";
        check
          (discovery.inventory_files = [Filename.concat root "linked-source"])
          "cleanup inventory retains a directory symlink as a leaf";
        check
          (List.mem
             (Test_support.path root "linked-source/Linked.res")
             discovery.present_files)
          "freshness inventory includes files below a directory symlink";
        File_util.ensure_dir (Filename.concat root "special");
        Unix.mkfifo (Test_support.path root "special/Blocked.res") 0o600;
        write_file
          (Test_support.path root "special/Regular.res")
          "let value = 1\n";
        write_file config_path
          {|{"name":"special-source","sources":["special"]}|};
        let discovered =
          Config.load config_path |> fun config -> discover config ()
        in
        check
          (names discovered = ["Regular"])
          "source discovery excludes FIFOs even when their names have source \
           extensions"))

let module_paths modules =
  List.map
    (fun (module_ : Source.module_) ->
      ( String.concat "." (module_.qualifier @ [module_.name]),
        module_.implementation,
        module_.interface ))
    modules

(* A project whose only source is a qualified src, holding [files]. *)
let qualified_project root files =
  List.iter
    (fun file -> write_file (Test_support.path root file) "let value = 1\n")
    files;
  let config_path = Filename.concat root "rescript.json" in
  write_file config_path
    {|{"name":"qualified","sources":{"dir":"src","subdirs":"qualified"}}|};
  Config.load config_path

let rejects ?filter config fragments =
  try
    ignore (discover config ?filter ());
    false
  with Source.Error message ->
    List.for_all (Test_support.contains_text message) fragments

(* Whether the filesystem at [root] tells file names apart by case. *)
let case_sensitive root =
  write_file (Test_support.path root "case-probe") "";
  let sensitive = not (Sys.file_exists (Test_support.path root "CASE-PROBE")) in
  Sys.remove (Test_support.path root "case-probe");
  sensitive

let qualified_tests _context =
  Test_support.with_temp_dir "rewatch-ocaml-qualified-" (fun root ->
      let config =
        qualified_project root
          [
            "src/App.res";
            "src/Ui/Button.res";
            "src/Ui/Button.resi";
            "src/Ui/Controls/Button.res";
            "src/Admin/Button.res";
            "src/Admin/Input.resi";
            "src/lower/thing.res";
            "src/.assets/notes.txt";
          ]
      in
      let orphans = ref [] in
      let discovery =
        Source.discover_with_inventory config ~on_missing:ignore
          ~on_orphan:(fun path -> orphans := path :: !orphans)
          ~prod:false ~features:None ~filter:None
      in
      assert_equal
        ~printer:(fun modules ->
          modules |> List.map (fun (name, _, _) -> name) |> String.concat ", ")
        [
          ("Admin.Button", "src/Admin/Button.res", None);
          ("App", "src/App.res", None);
          ("Lower.Thing", "src/lower/thing.res", None);
          ("Ui.Button", "src/Ui/Button.res", Some "src/Ui/Button.resi");
          ("Ui.Controls.Button", "src/Ui/Controls/Button.res", None);
        ]
        (module_paths discovery.modules);
      check
        (!orphans = ["src/Admin/Input.resi"])
        "an interface pairs only with the implementation in its directory";
      check
        (List.length (discover config ~filter:"^Button\\.res$" ()) = 3)
        "a filter matches same-named files in every directory");
  Test_support.with_temp_dir "rewatch-ocaml-qualified-" (fun root ->
      ignore (qualified_project root ["src/App.res"; "src/ui-kit/Button.res"]);
      write_file
        (Filename.concat root "rescript.json")
        {|{"name":"qualified","sources":{"dir":"./src","subdirs":"qualified"}}|};
      let config = Config.load (Filename.concat root "rescript.json") in
      check
        (rejects config ["ui-kit"; "is not a valid module name"])
        "a directory holding sources must have a valid module name";
      check
        (try
           ignore (discover config ());
           false
         with Source.Error message ->
           not
             (Test_support.contains_text message
                (Test_support.path Filename.current_dir_name "src/ui-kit")))
        "the directory is named without the source's \"./\"";
      check
        (rejects config ~filter:"^App\\.res$"
           ["ui-kit"; "is not a valid module name"])
        "a filter doesn't hide an invalid directory");
  Test_support.with_temp_dir "rewatch-ocaml-qualified-" (fun root ->
      let config = qualified_project root ["src/Ui.res"; "src/Ui/Button.res"] in
      check
        (rejects config ["Ui.res"; "both define module Ui."])
        "a file and a directory cannot define the same module");
  Test_support.with_temp_dir "rewatch-ocaml-qualified-" (fun root ->
      let config =
        qualified_project root ["src/Ui/Button.resi"; "src/Ui/Button/Icon.res"]
      in
      check
        (rejects config ["Button.resi"; "both define module Ui.Button."])
        "an interface and a directory cannot define the same module");
  Test_support.with_temp_dir "rewatch-ocaml-qualified-" (fun root ->
      if case_sensitive root then (
        let config = qualified_project root ["src/ui/A.res"; "src/Ui/B.res"] in
        check
          (rejects config ["are both module Ui."])
          "directories whose names differ only in case are rejected";
        let config =
          qualified_project root ["src/Ui/button.res"; "src/Ui/Button.res"]
        in
        check
          (rejects config ["Duplicate module name: Ui.Button"])
          "files whose names differ only in case are duplicates"))

let tests =
  "source_tests"
  >::: ["discovery" >:: discovery_tests; "qualified" >:: qualified_tests]
