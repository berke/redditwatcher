(* Mechanism *)

open Pffpsf
open Reddit

module Time =
  struct
    type t = float

    let origin = 0.0

    let compare = compare

    let now = Unix.gettimeofday

    let rec wait t0 =
      let t = now () in
      if t0 <= t then
        ()
      else
        let dt = t0 -. t in
        let _ = pf "Waiting for %f (t0=%f)\n%!" dt t0 in
        let _ = Unix.select [] [] [] dt in
        wait t
  end

type story = {
          st_reddit      : string; (* Base URL (reddit.com, science.reddit.com...) *)
          st_title       : string;
          st_url         : string;
          st_submitter   : string;
          st_id          : string;
  mutable st_details     : details option;
  mutable st_created     : Time.t;
  mutable st_history     : (Time.t * details) list;
}

type follow = {
  mutable fl_count : int;
  mutable fl_ids   : string list (* List of currently followed stories *)
}

type 'a context = {
          cx_pipe      : 'a;
  mutable cx_previous  : Time.t;                    (* Timestamp of previous HTTP request (to ensure that we don't hit the server too often) *)
          cx_url       : string;
          cx_stories   : (string, story) Hashtbl.t; (* Currently followed stories *)
  mutable cx_last_scan : Time.t;
          cx_queue     : story Queue.t
}

(** Load the story with the given ID *)
let load_story id =
  let fn = Filename.concat !Opt.story_dir (Util.sanitize_filename ~prefix:"story-" id) in
  if Sys.file_exists fn then
    try
      let st : story = Util.load fn in
      st
    with
    | _ -> raise Not_found
  else
    raise Not_found

(** Save the story with the given ID *)
let save_story cx st =
  let fn0 = Filename.concat !Opt.story_dir (Util.sanitize_filename ~prefix:"story-" st.st_id) in
  let fn1 = fn0^".bak" in
  Util.save fn1 st;
  if Sys.file_exists fn0 then Unix.unlink fn0;
  Unix.rename fn1 fn0

let with_doc ~cx ~url f =
  pf "With doc %s\n%!" url;
  let dt = Time.now () -. cx.cx_previous in
  if dt < !Opt.min_delay then
    begin
      let wt = !Opt.min_delay -. dt in
      pf "  Slowing down by %fs\n%!" wt;
      Time.wait (cx.cx_previous +. !Opt.min_delay)
    end;
  let doc = Www.obtain_document cx.cx_pipe (Www.Get url) in
  cx.cx_previous <- Time.now ();
  f doc

let scan_front cx =
  pf "Scanning %S\n%!" cx.cx_url;
  with_doc
    ~cx
    ~url:(cx.cx_url ^ "new")
    (fun doc -> process_front doc)

let update_story cx st =
  pf "Updating story %S %S\n%!" st.st_id st.st_title;
  with_doc
    ~cx
    ~url:
      (st.st_reddit ^
       "info/" ^
       st.st_id ^
       "/details")
    (fun doc ->
      let t = Time.now () in
      let de = process_details doc in
      if Some de <> st.st_details then
        begin
          st.st_history <- (t,de) :: st.st_history;
          save_story cx st
        end
    )

let details cx st =
  update_story cx st

let process_entry cx en =
  match en.en_id with
  | None -> pf "Story with no ID!\n%!"
  | Some id ->
      if not (Hashtbl.mem cx.cx_stories id) then
        let st =
          try
            load_story id
          with
          | Not_found ->
            (* See if the story has already been saved *)
            pf "New story %S %S\n%!" id en.en_title;
            let t = Time.now () in
            let st =
              {
                st_reddit      = cx.cx_url;
                st_title       = en.en_title;
                st_url         = en.en_url;
                st_submitter   = en.en_user;
                st_id          = id;
                st_created     = t;
                st_history     = [];
                st_details     = None
              }
            in
            save_story cx st;
            st
        in
        Hashtbl.add cx.cx_stories id st;
      else
        ()

let scan cx =
  pf "Executing scan\n%!";
  let entries = scan_front cx in
  Array.iteri (fun i en -> process_entry cx en) entries;
  cx.cx_last_scan <- Time.now ()

let controller url =
  let cx =
    {
      cx_pipe       = new Www.HC.pipeline;
      cx_previous   = Time.origin;
      cx_last_scan  = Time.origin;
      cx_stories    = Hashtbl.create 1000;
      cx_url        = url;
      cx_queue      = Queue.create ()
    }
  in

  while true do
    (* Decide which task to do next *)
    let t = Time.now () in
    if Hashtbl.length cx.cx_stories < !Opt.max_stories && t -. cx.cx_last_scan > !Opt.scan_interval then
      (* Do a scan *)
      scan cx
    else
      (* Do a story *)
      if Queue.is_empty cx.cx_queue then
        Time.wait (cx.cx_last_scan +. !Opt.scan_interval)
      else
        begin
          let st = Queue.take cx.cx_queue in
          details cx st;
          Queue.add st cx.cx_queue (* Add the story to the end of the queue *)
        end
  done
