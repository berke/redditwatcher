(* Www *)

open Neturl
open Nethtml
module HC = Http_client
open Pffpsf

type request =
| Get of string
| Post of string * (string * string) list
| File of string

exception Found of string

let http_url = Hashtbl.find common_url_syntax "http"

(*** concat_urls *)
let concat_urls u v =
  (* info (sf "Concat [%s] with [%s]" u v); *)
  string_of_url
  (apply_relative_url
  (url_of_string http_url u)
  (url_of_string (partial_url_syntax http_url) v))

(* ***)

let document_of_document_list x = Element("document-list",[],x)

let rec iter_over_data f = function
  | Data(x) -> f x
  | Element(what,attrs,cont) -> List.iter (iter_over_data f) cont


let rec iter_over_data_list f l = List.iter (iter_over_data f) l

(*** scan_elements *)
let rec scan_elements elt specs f = function
  | Data(_) -> ()
  | Element(what,attrs,cont) ->
      if what = elt &&
        List.for_all (fun (x,y) -> try y (List.assoc x attrs) with Not_found -> false) specs
      then
        f what attrs cont
      else
        List.iter (scan_elements elt specs f) cont

(* ***)

let scan_elements_list elt specs f l = List.iter (scan_elements elt specs f) l

let select_elements elt = List.filter (function Data(_) -> false | Element(x,y,z) -> x = elt)

(*** dump_document *)
let dump_document ch doc =
  let se = String.escaped in
  let o = Format.formatter_of_out_channel ch in
  let g = Format.fprintf in
  let rec h f sep flag = function
    | x::r ->
        if flag then g o "%s@ " sep;
        f x;
        h f sep true r
    | [] -> ()
  in let rec f = function
    Data(x) -> g o "\027[34mData\027[0m(\"\027[36m%s\027[0m\")" (se x)
  | Element(x,y,z) ->
      g o "@[<hov 2>\027[31mElement\027[0m(\"\027[31m%s\027[0m\",[" x;
      h (fun (x,y) -> g o "\027[34m%s\027[0m=\"%s\"" x (se y)) ";" false y;
      g o "],@,[";
      h f ";" false z;
      g o "])@]";
  in
  f doc; g o "@?"

(* ***)
(*** add_default_headers *)
let add_default_headers (m : HC.http_call) ?referer ?(cookies=[]) () =
  m # set_req_header
    "User-Agent"
    "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.8) Gecko/20060601 Firefox/2.0.0.8 (Ubuntu-edgy)";
  m # set_req_header
    "Accept"
    "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5";
  m # set_req_header "Accept-Language" "en-us,en;q=0.5";
  m # set_req_header "Accept-Charset" "UTF-8,*";
  (match referer with None -> () | Some(x) -> m # set_req_header "Referer" x);
  List.iter (fun (a,b) -> m # set_req_header "Cookie" (a^"="^b)) cookies

(* ***)
(*** obtain_document *)
let obtain_document (p : HC.pipeline) ?referer ?cookies request = 
  (*info (sf "Retrieving %s..." url);*)
  let from_file fn f =
    let ic = open_in fn in
    let nioc = new Netchannels.input_channel ic in
    try
      let x = f nioc in
      nioc#close_in ();
      x
    with
    | x ->
        nioc#close_in ();
        raise x
  in
  let from_net g =
    add_default_headers g ?referer ?cookies ();
    p # add g; p # run ();
    let (x,code,y) = g # dest_status () in
    match code with
    200 ->
      (*info (sf "Retrieval ok %s %s." x y);*)
      let body = g # get_resp_body () in
      new Netchannels.input_string body
    | x -> failwith (sf "obtain_document: got code %d" x)
  in
  let with_nioc f =
    match request with
    | Get url -> f (from_net (new HC.get url))
    | Post(url,extra) -> f (from_net (new HC.post url extra))
    | File name -> from_file name f
  in
  let doc_list = 
    with_nioc
      (fun nioc ->
        Nethtml.parse
          ~dtd:html40_dtd
          ~return_declarations:false
          ~return_pis:false
          ~return_comments:false
          nioc)
  in
  document_of_document_list doc_list
    (*(decode ~subst:(fun p -> Printf.sprintf "&#%d;" p))*)
    (*(decode doc_list) (**)
      ~subst:(fun p -> Printf.sprintf "&#%d;" p)
      ~enc:`Enc_usascii doc_list)*)
(* ***)
(*** add_to_pipe_and_parse *)
let add_to_pipe_and_parse p t url =
  p # add t; p # run ();
  let (x,code,y) = t # dest_status () in
  match code with
  | 200 ->
    (*info (sf "Retrieval ok %s %s." x y);*)
    let body = t # get_resp_body () in
    ignore body
  | x -> failwith (sf "add_to_pipe_and_parse: got code %d" x)

(* ***)
(*** parse_cookie *)
let parse_cookie a =
  let (b,_) = Util.split_once_at ((=) ';') a in
  Util.split_once_at ((=) '=') b 

(* ***)
(*** download *)
let download url fn =
  let module HC = Http_client.Convenience in
  Printf.printf "GETTING %s INTO %s\n%!" url fn;
  let msg = HC.http_get_message url in
  match msg#status with
  | `Successful ->
    begin
      let res = msg#response_body in
      let ic = res#open_value_rd () in
      let oc =
        try
          open_out_gen [Open_wronly;Open_creat;Open_trunc;Open_binary] 0o644 fn
        with
        | x ->
            Printf.eprintf "Error opening %S: %s.\n%!" fn (Printexc.to_string x);
            raise x
      in
      let tot = ref 0 in
      try
        let buf = String.create 4096 in
        while true do
          let m = ic#input buf 0 (String.length buf) in
          tot := !tot + m;
          output oc buf 0 m
        done
      with
      | End_of_file ->
          close_out oc;
          Printf.printf "Read %d bytes.\n%!" !tot
    end
  | `Redirection -> Printf.printf ">>> Redirection\n%!"
  | `Server_error -> Printf.printf ">>> Server error\n%!"
  | `Client_error -> Printf.printf ">>> Client error\n%!"
  | `Http_protocol_error x -> Printf.printf ">>> Protocol error %s\n%!" (Printexc.to_string x)
  | `Unserved -> Printf.printf ">>> Unserved\n%!"

(* ***)
