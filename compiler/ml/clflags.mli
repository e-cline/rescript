type mli_status = Mli_exists | Mli_non_exists

type t = {
  output_name: string option ref;
  include_dirs: string list ref;
  debug: bool ref;
  nopervasives: bool ref;
  all_ppx: string list ref;
  binary_annotations: bool ref;
  noassert: bool ref;
  verbose: bool ref;
  open_modules: string list ref;
  real_paths: bool ref;
  transparent_modules: bool ref;
  dump_source: bool ref;
  dump_parsetree: bool ref;
  dump_typedtree: bool ref;
  dump_rawlambda: bool ref;
  dump_coercions: bool ref;
  only_parse: bool ref;
  ignore_parse_errors: bool ref;
  dont_write_files: bool ref;
  keep_locs: bool ref;
  color: Misc.Color.setting option ref;
  assume_no_mli: mli_status ref;
  dont_record_crc_unit: string option ref;
  opened_namespaces: string list ref;
      (** Namespace maps opened by [-bs-open-ns]. As for
          [dont_record_crc_unit], their CRCs are not recorded, so adding a
          member to a map doesn't invalidate the interfaces that open it. *)
  bs_gentype: bool ref;
  jsx_preserve: bool ref;
      (** Keep JSX in the output ([-bs-jsx-preserve]); read by translcore, and
          by js_dump through [Js_config] *)
  no_assert_false: bool ref;
  dump_location: bool ref;
}

val current : unit -> t
(* Mutable compiler flags for the current domain. *)

val parse_color_setting : string -> Misc.Color.setting option
val reset_dump_state : unit -> unit

val reset : unit -> unit
(** Sets bsc's defaults: debug information and binary annotations on, colored
    diagnostics, and no locations in dumps. Fresh flags start from OCaml's
    defaults instead, which editor analysis relies on. *)
