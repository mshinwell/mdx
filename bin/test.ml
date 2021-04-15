(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Mdx

(* The two lines below are required to get the compiler to properly link
   all of the stubs for Unix and to allow successful dynamic loading of Lwt *)
external _exit : int -> 'a = "unix_exit"
let _ = if Sys.opaque_identity false then _exit 0 else ();;

let run_exn (`Setup ()) (`Non_deterministic non_deterministic)
    (`Silent_eval silent_eval) (`Record_backtrace record_backtrace)
    (`Syntax syntax) (`Silent silent) (`Verbose_findlib verbose_findlib)
    (`Prelude prelude) (`Prelude_str prelude_str) (`File file)
    (`Section section) (`Root root) (`Force_output force_output)
    (`Output output) =
  let output =
    match (output : Cli.output option) with
    | Some Stdout -> Some `Stdout
    | Some (File outfile) -> Some (`File outfile)
    | None -> None
  in
  let dirs = [] in
  let packages =
    [
      Mdx_test.Package.unix;
      Mdx_test.Package.findlib_internal;
    ]
  in
  let predicates = [ Mdx_test.Predicate.native ] in
  Mdx_test.run_exn ~non_deterministic ~silent_eval ~record_backtrace ~syntax
    ~silent ~verbose_findlib ~prelude ~prelude_str ~file ~section ~root
    ~force_output ~output ~dirs ~packages ~predicates

let report_error_in_block block msg =
  let kind =
    match block.Block.value with
    | Raw _ -> ""
    | Include { file_kind = Fk_ocaml _; _ } -> "OCaml file include "
    | Include { file_kind = Fk_other _; _ } -> "file include "
    | OCaml _ -> "OCaml "
    | Cram _ -> "cram "
    | Toplevel _ -> "toplevel "
  in
  Fmt.epr "%a: Error in the %scode block@]\n%s\n"
    Stable_printer.Location.print_loc block.loc kind msg

let run setup non_deterministic silent_eval record_backtrace syntax silent
    verbose_findlib prelude prelude_str file section root force_output output :
    int =
  try
    run_exn setup non_deterministic silent_eval record_backtrace syntax silent
      verbose_findlib prelude prelude_str file section root force_output output
  with
  | Failure f ->
      prerr_endline f;
      1
  | Mdx_test.Test_block_failure (block, msg) ->
      report_error_in_block block msg;
      1

(**** Cmdliner ****)

open Cmdliner

let cmd =
  let doc = "Test markdown files." in
  ( Term.(
      pure run $ Cli.setup $ Cli.non_deterministic $ Cli.silent_eval
      $ Cli.record_backtrace $ Cli.syntax $ Cli.silent $ Cli.verbose_findlib
      $ Cli.prelude $ Cli.prelude_str $ Cli.file $ Cli.section $ Cli.root
      $ Cli.force_output $ Cli.output),
    Term.info "test" ~doc )
