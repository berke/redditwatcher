(* Reddit *)

open Nethtml
open Scan
open Pffpsf

type entry =
  {
    mutable en_url : string;
    mutable en_title : string;
    mutable en_comments : string;
    mutable en_score : string
  }

let print_entry oc en =
  fp oc "URL: %S\n" en.en_url;
  fp oc "Title: %S\n" en.en_title;
  fp oc "Comments: %S\n" en.en_comments;
  fp oc "Score: %S\n" en.en_score

let process doc =
  let entries = ref [] in
  on_matching
    (has_class "entry")
    (fun e ->
      let en =
        {
          en_url = "";
          en_title = "";
          en_comments = "";
          en_score = ""
        }
      in
      (*pf "Entry: %a\n" Www.dump_document e;*)
      on_matching
        (element "a" &&& has_class "title")
        (fun e ->
          with_attribute "href" (fun u -> en.en_url <- u) () e;
          visit (on_data (fun u -> en.en_title <- u)) e;
          )
        e;
      on_matching
        (element "a" &&& has_class "bylink")
        (fun e ->
          with_attribute "href" (fun u -> en.en_comments <- u) () e
          )
        e;
      on_matching
        (element "span" &&& has_class "inside")
        (fun e ->
          visit (on_data (fun u -> en.en_score <- u)) e
        )
        e;
      entries := en :: !entries
    )
    doc;
  !entries
