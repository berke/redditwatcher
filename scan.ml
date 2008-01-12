(* Scan *)

open Nethtml

(** Combinators for extracting data from Xml documents *)

let rec visit f e =
  f e;
  match e with
  | Data _ -> ()
  | Element(_, _, xl) ->
      List.iter (visit f) xl

(** [element name] is a predicate that returns [true] iff the element is of
    kind name *)

let element u = function
  | Element(v, _, _) -> u = v
  | _ -> false

let with_attribute name f q = function
  | Data _ -> q
  | Element(_, attrs, _) ->
      try
        f (List.assoc name attrs)
      with
      | Not_found -> q

let on_matching f g e =
  visit
    (fun e ->
      if f e then g e
    )
  e

let alt f1 g1 f2 g2 e =
  visit
    (fun e ->
      if f1 e then
        (g1 e; true)
      else
        if f2 e then
          (g2 e; true)
        else
          false)

let on_data f = function
  | Data u -> f u
  | _ -> ()

let show_data e =
  visit
    (fun e -> on_data (fun u -> Printf.printf "%S\n%!" u) e; false)
    e

let get_classes e = with_attribute "class" (fun cl -> Util.split_at ' ' cl) [] e

let has_class cl e = List.mem cl (get_classes e)

let ( &&& ) f1 f2 e = f1 e && f2 e

