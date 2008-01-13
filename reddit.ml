(* Reddit *)

open Nethtml
open Scan
open Print
open Pffpsf

type entry =
  {
    mutable en_url : string;
    mutable en_title : string;
    mutable en_user : string;
    mutable en_id : string option;
    mutable en_comments : int option;
    mutable en_score : int option
  }

type details =
  {
    mutable de_up_votes : int;
    mutable de_down_votes : int;
    mutable de_comments : int;
  }

let print_entry oc en =
  fp oc "URL: %S\n" en.en_url;
  fp oc "Title: %S\n" en.en_title;
  fp oc "User: %S\n" en.en_user;
  fp oc "Comments: %a\n" (print_option print_int) en.en_comments;
  fp oc "ID: %a\n" (print_option print_string) en.en_id;
  fp oc "Score: %a\n" (print_option print_int) en.en_score

let score_rex = Pcre.regexp "^([0-9]+) "
let id_from_comments_rex = Pcre.regexp "info/([^/]+)/comments"
let num_comments_rex = Pcre.regexp "\\S([0-9]+) *comment"
let user_rex = Pcre.regexp "^/user/"

let process_front doc =
  let entries = ref [] in
  on_matching
    (has_class "entry")
    (fun e ->
      let en =
        {
          en_url = "";
          en_user = "";
          en_title = "";
          en_id = None;
          en_comments = None;
          en_score = None
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
          with_attribute "href" (fun u ->
            begin
              try
                en.en_id <- Some(Pcre.get_substring (Pcre.exec ~rex:id_from_comments_rex u) 1)
              with
              | Not_found -> ()
            end) () e;
          visit
            (on_data
              (fun u ->
                try
                  en.en_comments <- Some(int_of_string (Pcre.get_substring (Pcre.exec ~rex:num_comments_rex u) 1))
                with
                | _ -> ()
              )
            )
            e
          )
        e;
      on_matching
        (element "div" &&& has_class "little")
        (on_matching
          (element "a" &&&
            (with_attribute "href"
              (Pcre.pmatch ~rex:user_rex)
              false))
          (visit (on_data (fun u -> en.en_user <- u))))
        e;
      on_matching
        (element "span" &&& has_class "inside")
        (fun e ->
          visit
            (on_data
              (fun u ->
                try
                  en.en_score <- Some(int_of_string (Pcre.get_substring (Pcre.exec ~rex:score_rex u) 1))
                with
                | _ -> ())
              )
            e
        )
        e;
      entries := en :: !entries
    )
    doc;
  Array.of_list (List.rev !entries)

let process_details doc =
  let de =
    {
      de_up_votes = 0;
      de_down_votes = 0;
      de_comments = 0;
    }
  in
  let extractor name f =
    (on_matching
       (element "tr" &&& has (data_matches ((=) name)))
       (on_matching
          (element "td" &&& !!! (has_class "profline"))
          (visit (on_data (fun u -> f (int_of_string u))))
       )
    )
  in
  on_matching
    (has_class "details")
    (fun e ->
      extractor "up votes" (fun x -> de.de_up_votes <- x) e;
      extractor "down votes" (fun x -> de.de_down_votes <- x) e;
    )
    doc;

  on_matching
    (has_class "entry")
    (on_matching
      (element "a" &&& has_class "bylink")
      (visit
         (on_data
           (fun u ->
             try
               de.de_comments <- int_of_string (Pcre.get_substring (Pcre.exec ~rex:num_comments_rex u) 1)
             with
             | _ -> ()
           )
         )
      )
    )
    doc;

  de
