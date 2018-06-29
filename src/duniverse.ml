(* Copyright (c) 2018 Anil Madhavapeddy <anil@recoil.org>
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
 *
 *)

open Cmdliner

let copts = ()

let setup_logs () =
  Printexc.record_backtrace true ;
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer () ;
    Logs.set_level level ;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  let global_option_section = "COMMON OPTIONS" in
  let open Term in
  const setup_log
  $ Fmt_cli.style_renderer ~docs:global_option_section ()
  $ Logs_cli.level ~docs:global_option_section ()

let copts_t =
  let _docs = Manpage.s_common_options in
  Term.(const copts)

let fpath_t = Arg.conv ~docv:"PATH" (Fpath.of_string, Fpath.pp)

let target_repo_t =
  let doc = "Path to Git repository to store vendored code in." in
  let open Arg in
  value
  & opt fpath_t (Fpath.v ".")
  & info ["r"; "--repo"] ~docv:"TARGET_REPO" ~doc

let branch_t =
  let doc =
    "Target branch to switch to before merging in the vendored repositories. \
     If not supplied, the current branch in the target repository is used."
  in
  Arg.(value & opt (some string) None & info ["b"] ~docv:"VENDOR_BRANCH" ~doc)

let opam_cmd =
  let doc = "opam TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  let pkg_t =
    let open Arg in
    non_empty & pos_all string []
    & info [] ~doc:"opam packages to calculate duniverse for" ~docv:"PACKAGES"
  in
  let exclude_t =
    let doc =
      "Packages to exclude from the output list. You can use this to remove \
       the root packages so they are not duplicated in the vendor directory.  \
       Repeat this flag multiple times for more than one exclusion."
    in
    Arg.(value & opt_all string [] & info ["exclude"; "x"] ~docv:"EXCLUDE" ~doc)
  in
  let pins_t =
    let doc =
      "Packages to pin to their master branches for the latest opam metadata \
       and source. Repeat this flag multiple times for more than one \
       exclusion."
    in
    Arg.(value & opt_all string [] & info ["pin"; "p"] ~docv:"PIN" ~doc)
  in
  ( (let open Term in
    term_result
      ( const Opam_cmd.init_duniverse
      $ target_repo_t $ pkg_t $ exclude_t $ pins_t $ setup_logs () ))
  , Term.info "opam" ~doc ~exits ~man )

let dune_lock_cmd =
  let doc = "dune-lock TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( (let open Term in
    term_result (const Dune_cmd.gen_dune_lock $ target_repo_t $ setup_logs ()))
  , Term.info "lock" ~doc ~exits ~man )

let dune_lockfile_t =
  let doc = "Input path of Dune lockfile" in
  let open Arg in
  value
  & opt fpath_t Config.duniverse_lockfile
  & info ["f"] ~docv:"DUNE_LOCKFILE" ~doc

let dune_fetch_cmd =
  let doc = "dune-fetch TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( (let open Term in
    term_result
      ( const Dune_cmd.gen_dune_upstream_branches
      $ target_repo_t $ branch_t $ setup_logs () ))
  , Term.info "pull" ~doc ~exits ~man )

let status_cmd =
  let doc = "status TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( (let open Term in
    term_result
      (const Dune_cmd.status $ target_repo_t $ branch_t $ setup_logs ()))
  , Term.info "status" ~doc ~exits ~man )

let default_cmd =
  let doc = "duniverse is the spice of build life" in
  let sdocs = Manpage.s_common_options in
  let man = [`S Manpage.s_description] in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "duniverse" ~version:"%%VERSION%%" ~doc ~sdocs ~man )

let cmds = [opam_cmd; dune_lock_cmd; dune_fetch_cmd; status_cmd]

let () = Term.(exit @@ eval_choice default_cmd cmds)
