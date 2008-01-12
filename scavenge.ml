(* Monitor *)
(* Copyright (C)2005-2006 Berke Durak *)
(* Released under the GNU General Public License, version 2 or later. *)

open Util
open Bool
open Target

exception Alarm

let continue = ref false

let default_entry = {
  name = "unnamed";
  delay = 300.0;
  bad_max = 1;
  test = False;
  action = [];
  bad_count = 0;
  last_test = 0.0;
  is_good = true;
  timeout = 30
}

(*** swap *)
let swap a i j =
  let x = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- x

(* ***)
(*** randomize *)
let randomize a =
  let m = Array.length a in
  for i = 1 to m - 1 do
    let j = i + Random.int (m - i) in
    swap a i j
  done

(* ***)


module Do =
  struct
    (*** download *)
    let download low =
      let base = "data" in
      let count = 14000 in
      let low = 7000006 in
      let a = Array.make count 0 in
      for i = 0 to count - 1 do
        a.(i) <- low + i
      done;
      randomize a;
      for i = 0 to count - 1 do
        let fn = Filename.concat base (sf "a%08d.html" a.(i)) in
        if Sys.file_exists fn then
          Printf.printf "Skipping %08d\n%!" a.(i)
        else
          begin
            let url =
              sf "http://cadres.apec.fr/offres-emploi-cadres/0_%d____0____offre-emploi-ingenieur-developpement-standards-h-f.html\n" a.(i)
            in
            let part = fn^".part" in
            try
              Www.download url part;
              Sys.rename part fn;
            with
            | x ->
                Printf.printf ">>> EXCEPTION: %s\n%!" (Printexc.to_string x);
                (try
                  Sys.remove part
                with
                | _ -> ())
          end
      done
    
    (* ***)
    (*** grep_document *)
    let grep_document url regex =
      let p = new Www.HC.pipeline in
      let rex = Pcre.regexp ~study:true regex in
      let doc = Www.obtain_document p (Www.File url) in
      (*with_output_to_file "unfiltered.txt" (fun f -> dump_document f doc); *)
      Www.iter_over_data
        begin fun w ->
          if Pcre.pmatch ~rex w then
            Printf.printf "Matched %S.\n" w
          else
            ()
        end
        doc
    
    (* ***)
    (*** scan_offer *)
    let scan_offer fn =
      let p = new Www.HC.pipeline in
      let doc = Www.obtain_document p (Www.File fn) in
      let under_element elt f doc = Www.scan_elements elt [] (fun what attr cont -> List.iter f cont) doc in
      let under_class cls f doc =
        Www.scan_elements "div" ["class", ( (=) cls )] (fun what attr cont -> List.iter f cont) doc
      in
      Printf.printf "%s" fn;
      under_class "contentWithDashedBorderTop backgroundColored boxContent"
        (under_class "boxContentInside"
          (under_element "p"
            (Www.iter_over_data
              begin fun d ->
                Printf.printf " %s" d
              end)))
        doc;
      Printf.printf "\n"
    
    (* ***)
    (*** scan_offers *)
    let scan_offers fn =
      let st = Unix.stat fn in
      match st.Unix.st_kind with
      | Unix.S_DIR ->
          begin
            let d = Unix.opendir fn in
            try
              while true do
                let r = Unix.readdir d in
                match r with
                | "."|".." -> ()
                | n -> scan_offer (Filename.concat fn n)
              done
            with
            | End_of_file ->
                Unix.closedir d
          end
      | _ -> scan_offer fn
    
    (* ***)
    (*
    (*** bool_command *)
    let bool_command cmd formula =
      try
        let ic = Unix.open_process_in cmd in
        let formula = Bool.map (fun (_,rex) -> (rex, ref false)) formula in
        (*with_output_to_file "unfiltered.txt" (fun f -> dump_document f doc); *)
        try
          while true do
            let l = input_line ic in
            ignore (Bool.iter (fun (rex, result) ->
              if Pcre.pmatch ~rex l then
                begin
                  result := true
                end) formula)
          done;
          assert false
        with
        | End_of_file ->
            ignore (Unix.close_process_in ic);
            Bool.eval (fun (rex, result) -> !result) formula;
      with
      | x ->
          msg (sf "While executing %S, exception %s" cmd (Printexc.to_string x));
          false
    
    (* ***)
    (*** bool_document *)
    let bool_document url formula =
      if !Opt.debug then
        Printf.eprintf "Checking URL %S\n%!" url;
      let p = new Www.HC.pipeline in
      try
        (*let formula = Bool.map (Pcre.regexp ~study:true) formula in*)
        let doc = Www.obtain_document [] p None url in
        (*with_output_to_file "unfiltered.txt" (fun f -> dump_document f doc); *)
        let res =
          Bool.eval
            (fun (rs,rex) ->
              let result = ref false in
              Www.iter_over_data
                begin fun w ->
                  let outcome = Pcre.pmatch ~rex w in
                  if !Opt.debug then
                    Printf.eprintf "Match(%S,%S)=%b\n%!" rs w outcome;
                  if outcome then
                    begin
                      result := true;
                    end
                end
                doc;
              !result) formula
        in
        p#reset ();
        res
      with
      | x ->
          p#reset ();
          msg (sf "While checking %S, exception %s" url (Printexc.to_string x));
          false
    
    (* ***)
    (*** perform_test *)
    let perform_test e =
       try
         with_timeout e.timeout
           begin
             Bool.eval
               begin function
                 | Url url,formula -> bool_document url formula
                 | Cmd cmd,formula -> bool_command cmd formula
               end
           end
           e.test
       with
       | Alarm -> false
    
    (* ***)
    (*** check *)
    let check tests =
      List.iter
        begin fun e ->
          let t = Unix.gettimeofday () in
          if e.last_test = 0.0 or t -. e.last_test > e.delay then
            begin 
              msg (sf "Testing for %S" e.name);
              e.last_test <- t;
              let result = perform_test e in
              if e.is_good then
                begin
                  if result then
                    begin
                      e.bad_count <- 0;
                      msg (sf "%S is okay" e.name)
                    end
                  else
                    begin
                      e.bad_count <- e.bad_count + 1;
                      if e.bad_count >= e.bad_max then
                        begin
                          msg (sf "Houston, we have a problem on %S" e.name);
                          e.is_good <- false;
                          if not !continue then Action.perform_action e.is_good e
                        end
                      else
                        msg (sf "There seems to be a problem on %S, count = %d" e.name e.bad_count);
                    end
                end
              else
                begin
                  if result then
                    begin
                      msg (sf "%S is good again" e.name);
                      e.bad_count <- 0;
                      e.is_good <- true;
                      if not !continue then Action.perform_action e.is_good e
                    end
                  else
                    begin
                      e.bad_count <- e.bad_count + 1;
                      if e.bad_count >= e.bad_max then
                        begin
                          msg (sf "Houston, we still have a problem on %S" e.name);
                          e.is_good <- false;
                        end
                      else
                        msg (sf "There seems to be a problem on %S, count = %d" e.name e.bad_count);
                    end
                end
            end
          else
            ()
        end
        tests
    
    (* ***)
    (*** forever *)
    let forever tests =
      Sys.set_signal
        Sys.sigalrm
        (Sys.Signal_handle(fun _ -> raise Alarm));
      while true do
        check tests;
        continue := false;
        Unix.sleep 5
      done
    
    (* ***)
    *)
    (*** dump_document *)
    let dump_document url =
      let p = new Www.HC.pipeline in
      let doc = Www.obtain_document p (Www.File(url)) in
      (*with_output_to_file "unfiltered.txt" (fun f -> dump_document f doc); *)
      Www.dump_document stdout doc
    
    (* ***)
  (*** testify *)
  let testify 
    (l : (string *
          (Target.source * (string * Pcre.regexp) Bool.boolean) list *
          (int * Target.action list))
            list)
    =
    List.map
      begin fun (name, tests, (bad_max, actions)) ->
        let t = And(List.map (fun x -> Atom x) tests) in
        { default_entry with
          delay = !Opt.delay;
          bad_max = bad_max;
          action = actions;
          name = name;
          test = t }
      end
      l
  
  (* ***)
  end

(* ***)

(*** spec *)
let spec = [
  "-continue", Arg.Set continue, " continue processing ; don't re-send alerts";
  "-dump", Arg.String(fun url -> Do.dump_document url), "<url> Retrieve HTML document and dump its syntax tree.";
  "-scan", Arg.String(fun url -> Do.scan_offers url), "<url> Scan job offers.";
  "-download", Arg.Unit(fun () -> Do.download ()), " Download job offers.";
  "-config-file", Arg.Set_string Opt.config_file, sf "<path> Set configuration file (%s)" !Opt.config_file;
  "-debug", Arg.Set Opt.debug, " Enable debugging";
  "-grep",
    Arg.Tuple(
      let url = ref "" in
      [Arg.Set_string url;
       Arg.String(fun regex -> Do.grep_document !url regex)]),
    "<url> <regex>  Download and grep document.";
  "-delay", Arg.Set_float Opt.delay, "<delay> Delay between tests in seconds.";
  "-sendmail", Arg.Set_string Opt.sendmail, sf "<sendmail> Set sendmail command (%S)" !Opt.sendmail
]
(* ***)

let _ =
  Arg.parse spec
    (*(fun _ -> ())*)
    Do.scan_offers
    (sf "Usage: %s [options]" (Filename.basename Sys.argv.(0)));
  (*let cfg = Parser.load !Opt.config_file in
  let tests = Do.testify cfg in
  Do.forever tests*)


