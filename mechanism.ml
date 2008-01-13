(* Mechanism *)

open Pffpsf
open Reddit

module Time =
  struct
    type t = float

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

module TM = Map.Make(Time)

type event =
  | Start
  | Delete
  | Upmod
  | Downmod
  | Comment
  | Uncomment

type story = {
          st_reddit      : string; (* Base URL (reddit.com, science.reddit.com...) *)
          st_title       : string;
          st_url         : string;
          st_submitter   : string;
          st_id          : string;
  mutable st_created     : Time.t;
  mutable st_last_event  : Time.t;
  mutable st_predicted   : Time.t; (* When next event is predicted *)
  mutable st_up_votes    : int option;
  mutable st_down_votes  : int option;
  mutable st_comments    : int option;
  mutable st_score       : int option;
}

type t = {
  tk_name     : string;
  tk_schedule : Time.t;
  tk_exec     : (adder -> unit);
}
and adder = (t -> unit)

type 'a context = {
  cx_pipe    : 'a;
  cx_url     : string;
  cx_stories : (string, story) Hashtbl.t;
  cx_add     : adder
}

module TQ = Prioqueue.Make(Time)

let scheduler initial =
  let tasks = TQ.create () in
  let add t =
    let _ = TQ.add tasks t t.tk_schedule in
    ()
  in

  (* Add initial task *)
  add initial;

  while not (TQ.is_empty tasks) do
    let tk = TQ.data (TQ.take tasks) in
    let t = Time.now () in
    let dt = tk.tk_schedule -. t in
    if dt > 0.0 then pf "Next task %s in %fs\n%!" tk.tk_name dt;
    Time.wait tk.tk_schedule;
    tk.tk_exec add
  done

let with_doc ~cx ~url f =
  let doc = Www.obtain_document cx.cx_pipe (Www.Get url) in
  f doc

let scan_front cx =
  pf "Scanning %S\n%!" cx.cx_url;
  with_doc
    ~cx
    ~url:cx.cx_url
    (fun doc -> process_front doc)

let process_entry cx en =
  match en.en_id with
  | None -> pf "Story with no ID!\n%!"
  | Some id ->
      if not (Hashtbl.mem cx.cx_stories id) then
        begin
          pf "New story %S\n%!" id;
          let t = Time.now () in
          let st =
            {
              st_reddit      = cx.cx_url;
              st_title       = en.en_title;
              st_url         = en.en_url;
              st_submitter   = en.en_user;
              st_id          = id;
              st_created     = t;
              st_last_event  = t;
              st_predicted   = t +. !Opt.delay;
              st_up_votes    = None;
              st_down_votes  = None;
              st_comments    = en.en_comments;
              st_score       = en.en_score;
            }
          in
          Hashtbl.add cx.cx_stories id st
    end

let perform url =
  let scan_delay = 10.0 in

  let rec scan_task cx schedule =
    {
      tk_name     = "Scan";
      tk_schedule = schedule;
      tk_exec     =
        (fun add ->
          pf "Executing scan\n%!";
          let entries = scan_front cx in
          List.iter (process_entry cx) entries;
          add (scan_task cx (Time.now () +. scan_delay))
        )
    }
  in

  let initial add =
    let cx =
      {
        cx_pipe    = new Www.HC.pipeline;
        cx_stories = Hashtbl.create 1000;
        cx_add     = add;
        cx_url     = url;
      }
    in
    add (scan_task cx 0.0)
  in

  scheduler
    {
      tk_name     = "Init";
      tk_schedule = 0.0;
      tk_exec     = initial
    }
