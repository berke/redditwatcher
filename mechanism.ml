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

type event_type =
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
  mutable st_details     : details option;
  mutable st_history     : (Time.t * details) list;
}

type t = {
          tk_name     : string;
  mutable tk_schedule : Time.t;
          tk_exec     : (adder -> unit);
}
and adder = (t -> unit)

type 'a context = {
          cx_pipe     : 'a;
  mutable cx_previous : Time.t;
          cx_url      : string;
          cx_stories  : (string, story) Hashtbl.t;
          cx_add      : adder
}

module TQ = Prioqueue.Make(Time)

let scheduler initial =
  let t0 = Time.now () in

  let delay = ref 0.0 in

  let tasks = TQ.create () in
  let add t =
    if not (TQ.is_empty tasks) then
      begin
        let t_next = !delay +. TQ.priority (TQ.get tasks 0) in
        if t.tk_schedule -. t_next < !Opt.min_delay then
          t.tk_schedule <- t_next +. !Opt.min_delay;
      end;
    let _ = TQ.add tasks t t.tk_schedule in
    ()
  in

  (* Add initial task *)
  add initial;

  let show_tasks () =
    let t = Time.now () in
    pf "Tasks (t=%f):\n" (t -. t0);
    TQ.iter
      (fun i _ tk ->
        let ts = tk.tk_schedule +. !delay in
        pf "  #%d: %s @ %f (%f)\n" i tk.tk_name (ts -. t0) (ts -. t)
      )
      tasks;
    pf "%!"
  in

  while not (TQ.is_empty tasks) do
    show_tasks ();
    let tk = TQ.data (TQ.take tasks) in
    let t = Time.now () in
    pf "Delay %f\n" !delay;
    let dt = tk.tk_schedule +. !delay -. t in
    if dt > 0.0 then
      begin
        pf "Next task %s in %fs\n%!" tk.tk_name dt;
        Time.wait (tk.tk_schedule +. !delay)
      end
    else
      if tk.tk_schedule > 0.0 then
        begin
          pf "Next task %s is late by %fs\n%!" tk.tk_name (-. dt);
          delay := !delay -. dt
        end;
    pf ">>> Execute %s\n%!" tk.tk_name;
    tk.tk_exec add
  done

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
      st.st_history <- (t,de) :: st.st_history;
      let t = Time.now () in
      st.st_predicted <- t +. !Opt.min_delay +. 2.0 *. Random.float !Opt.average_delay
    )

let rec details cx st add =
  update_story cx st;
  cx.cx_add
    {
      tk_name     = sf "Details %S" st.st_title;
      tk_schedule = st.st_predicted;
      tk_exec     = details cx st
    }

let process_entry cx delay en =
  match en.en_id with
  | None -> pf "Story with no ID!\n%!"
  | Some id ->
      if not (Hashtbl.mem cx.cx_stories id) then
        begin
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
              st_last_event  = t;
              st_predicted   = t +. delay +. Random.float !Opt.average_delay;
              st_details     = None;
              st_history     = []
            }
          in
          Hashtbl.add cx.cx_stories id st;
          cx.cx_add
            {
              tk_name     = sf "Initial details %S" st.st_title;
              tk_schedule = st.st_predicted;
              tk_exec     = details cx st
            }
    end

let perform url =
  let scan_delay = 60.0 in

  let rec scan_task cx schedule =
    {
      tk_name     = "Scan";
      tk_schedule = schedule;
      tk_exec     =
        (fun add ->
          pf "Executing scan\n%!";
          let entries = scan_front cx in
          Array.iteri (fun i en -> process_entry cx (!Opt.min_delay +. float i *. !Opt.average_delay) en) entries;
          add (scan_task cx (Time.now () +. scan_delay))
        )
    }
  in

  let initial add =
    let cx =
      {
        cx_pipe     = new Www.HC.pipeline;
        cx_previous = 0.0;
        cx_stories  = Hashtbl.create 1000;
        cx_add      = add;
        cx_url      = url;
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
