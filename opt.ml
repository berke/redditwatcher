(* Opt *)

let story_dir = ref "stories"
let min_delay = ref 3.0
let average_delay = ref 10.0
let delay = ref 600.0
let config_file = ref "monitor.config"
let debug = ref false
let sendmail = ref "/usr/sbin/sendmail ${WHO}"
let contact = ref "root@localhost"
