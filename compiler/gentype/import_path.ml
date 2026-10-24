open Gentype_common

type t = string * string

let bs_curry_path ~config = ("", Config.get_bs_curry_path ~config)

let from_module ~dir ~import_extension module_name =
  let with_no_path =
    (module_name |> Module_name.to_string
   |> Scoped_package.remove_generated_module)
    ^ import_extension
  in
  (dir, with_no_path)

let from_string_unsafe s = ("", s)

let chop_extension_safe (dir, s) =
  try (dir, s |> Filename.chop_extension) with Invalid_argument _ -> (dir, s)

let dump (dir, s) = Node_filename.concat dir s

(* [path] without "." segments, and without ".." segments after a directory. *)
let normalize path =
  let rec loop acc = function
    | [] -> List.rev acc
    | "." :: rest -> loop acc rest
    | ".." :: rest -> (
      match acc with
      | directory :: acc when directory <> ".." -> loop acc rest
      | _ -> loop (".." :: acc) rest)
    | segment :: rest -> loop (segment :: acc) rest
  in
  String.split_on_char '/' path |> loop [] |> String.concat "/"

let to_cmt ~(config : Config.t) ~output_file_relative (dir, s) =
  let open Filename in
  let source =
    concat
      (output_file_relative |> dirname)
      ((dir, s) |> chop_extension_safe |> dump)
  in
  match Qualified_modules.unit_of_source ~config (normalize source) with
  | Some unit_name -> concat (dirname source) unit_name ^ ".cmt"
  | None ->
    source
    ^ (match config.namespace with
      | None -> ""
      | Some name -> "-" ^ name)
    ^ ".cmt"

(* Import paths are emitted inside single-quoted JavaScript/TypeScript string
   literals and also repeated in line comments. The AST stores their semantic
   value, so restore source escapes at this final output boundary. Escaping the
   Unicode line separators keeps them from terminating those comments. *)
let escape_for_single_quotes s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i < len then
      (* The UTF-8 encodings of U+2028 and U+2029 differ only in their final
         byte. Preserve all other UTF-8 text verbatim. *)
      if
        i + 2 < len
        && s.[i] = '\226'
        && s.[i + 1] = '\128'
        && (s.[i + 2] = '\168' || s.[i + 2] = '\169')
      then (
        Buffer.add_string buf
          (if s.[i + 2] = '\168' then "\\u2028" else "\\u2029");
        loop (i + 3))
      else (
        (match s.[i] with
        | '\'' -> Buffer.add_string buf "\\'"
        | '\\' -> Buffer.add_string buf "\\\\"
        | '\b' -> Buffer.add_string buf "\\b"
        | '\012' -> Buffer.add_string buf "\\f"
        | '\n' -> Buffer.add_string buf "\\n"
        | '\r' -> Buffer.add_string buf "\\r"
        | '\t' -> Buffer.add_string buf "\\t"
        | c when Char.code c < 0x20 || Char.code c = 0x7f ->
          Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c))
        | c -> Buffer.add_char buf c);
        loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let emit path = path |> dump |> escape_for_single_quotes
