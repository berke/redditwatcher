(* Opt *)

let delay = ref 60.0;;
let config_file = ref "monitor.config";;
let debug = ref false;;
let sendmail = ref "/usr/sbin/sendmail ${WHO}";;
let contact = ref "root@localhost";;
