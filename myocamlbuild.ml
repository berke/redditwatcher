(* Myocamlbuild for Jsure *)

open Ocamlbuild_pack;;
open Ocamlbuild_plugin;;
open Command;;
open Ocaml_specific;;

let ocamlfind_query pkg =
  let cmd = Printf.sprintf "ocamlfind query %s" (Filename.quote pkg) in
  My_unix.run_and_open cmd (fun ic ->
    Log.dprintf 5 "Getting Ocaml directory from command %s" cmd;
    input_line ic)

let aurochs = ref (S[A"aurochs";A"-quiet";A"-target";A"ml"]);;
let system_lib_dir = "/usr/lib";;

dispatch
  begin function
  | After_rules ->
      begin
        List.iter
          begin fun pkg ->
            let dir = ocamlfind_query pkg in
            ocaml_lib ~extern:true ~dir:dir pkg;
            flag ["compile"; "ocaml"; "use_"^pkg] (S[A"-I";A dir])
          end
          [
           "pcre";
           "equeue";
           "netsys";
           "netstring";
           (*"neturl";*)
           "netclient";
           "nethttpd";
            ];

        Options.ocamlopt := S[A"ocamlopt";A"-verbose"];

        flag ["link"; "ocaml"; "byte"; "use_libaurochs"]
             (S[A"-dllib";A("-laurochs"); A"-cclib";A("-laurochs")]);

        flag ["link"; "ocaml"; "use_libaurochs"]
             (S[A"-ccopt"; A("-L"^system_lib_dir); A"-cclib"; A"-laurochs"]);

        rule "aurochs: .peg -> .ml,.mli"

        ~prods:["%.ml";"%.mli"]

        ~dep:"%.peg"
          begin fun env _build ->
            let peg = env "%.peg" and ml = env "%.ml" in
            let tags = tags_of_pathname ml++"aurochs" in
            Cmd(S[!aurochs; T tags; P peg])
          end
      end
  | _ -> ()
  end
