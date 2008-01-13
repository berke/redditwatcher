(* Mechanism *)

open Pffpsf

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
  mutable st_up_votes    : int;
  mutable st_down_votes  : int;
  mutable st_comments    : int;
}

type t = {
  tk_name     : string;
  tk_schedule : Time.t;
  tk_exec     : (adder -> unit);
}
and adder = (t -> unit)

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

let perform url =
  let pipe = new Www.HC.pipeline in
  let with_doc url f =
    let doc = Www.obtain_document pipe (Www.Get url) in
    f doc
  in

  let rec scan_task schedule =
    {
      tk_name     = "Scan";
      tk_schedule = schedule;
      tk_exec     =
        (fun add ->
          pf "Executing scan\n%!";
          with_doc
            url
            (fun doc ->
              let entries = Reddit.process_front doc in
              List.iter (Reddit.print_entry stdout) entries;
              add (scan_task (Time.now () +. 10.0))
            )
        )
    }
  in
  scheduler (scan_task 0.0);
