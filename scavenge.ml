(* Monitor *)
(* Copyright (C)2005-2006 Berke Durak *)
(* Released under the GNU General Public License, version 2 or later. *)

open Pffpsf
open Bool
open Target

let continue = ref false

module Do =
  struct
    let dump url =
      let p = new Www.HC.pipeline in
      let doc = Www.obtain_document p (Www.Get url) in
      Marshal.to_channel stdout doc []

    let scan url =
      let p = new Www.HC.pipeline in
      let doc = Www.obtain_document p (Www.Get url) in
      let entries = Reddit.process_front doc in
      List.iter (Reddit.print_entry stdout) entries
  end

let spec = [
  "-scan", Arg.String(fun url -> Do.scan url), "<url> Scan entry.";
  "-dump", Arg.String(fun url -> Do.dump url), "<url> Dump URL content to stdout.";
  "-debug", Arg.Set Opt.debug, " Enable debugging.";
  "-delay", Arg.Set_float Opt.delay, "<delay> Delay between tests in seconds.";
]

let _ =
  Arg.parse spec
    Mechanism.perform
    (sf "Usage: %s [options]" (Filename.basename Sys.argv.(0)));
