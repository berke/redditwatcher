(* Print *)

open Pffpsf

let print_string = output_string

let print_int oc x = fp oc "%d" x

let print_option f oc = function
  | None -> fp oc "_"
  | Some u -> f oc u
