(* Scavenge *)

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
      Array.iter (Reddit.print_entry stdout) entries

    open Mechanism
    open Reddit

    let show fn =
      let st : story = Util.load fn in
      let oc = stdout in
      fp oc "# Reddit: %s\n" st.st_reddit;
      fp oc "# Title: %S\n" st.st_title;
      fp oc "# URL: %S\n" st.st_url;
      fp oc "# Id: %s\n" st.st_id;
      List.iter
        (fun (t,de) ->
          fp oc "%11.0f %d %d %d\n" t de.de_up_votes de.de_down_votes de.de_comments
        )
        st.st_history
  end

let spec = [
  "-dir",           Arg.Set_string Opt.story_dir, "<dir> Directory for saving stories.";
  "-max-stories",   Arg.Set_int Opt.max_stories, "<n> Set maximum number of new stories to follow.";
  "-scan",          Arg.String(fun url -> Do.scan url), "<url> Scan entry.";
  "-dump",          Arg.String(fun url -> Do.dump url), "<url> Dump URL content to stdout.";
  "-show",          Arg.String(fun fn -> Do.show fn),   "<fn> Show a given story";
  "-debug",         Arg.Set Opt.debug, " Enable debugging.";
  "-id",            Arg.String(fun u -> Opt.extra_ids := u :: !Opt.extra_ids), "<id> Add extra story to track";
  "-min-delay",     Arg.Set_float Opt.min_delay, "<delay> Minimum delay between HTTP requests in seconds.";
  "-scan-interval", Arg.Set_float Opt.scan_interval, "<delay> Delay between scans of the new stories page."
]

let _ =
  Arg.parse spec
    Mechanism.controller
    (sf "Usage: %s [options]" (Filename.basename Sys.argv.(0)));
