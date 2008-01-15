#!/bin/sh

exec ledit -x -h .ledit ocaml unix.cma -I /usr/local/lib/ocaml/site-lib/pcre -I /usr/local/lib/ocaml/site-lib/equeue -I /usr/local/lib/ocaml/site-lib/netsys -I /usr/local/lib/ocaml/site-lib/netstring -I /usr/local/lib/ocaml/site-lib/neturl -I /usr/local/lib/ocaml/site-lib/netclient -I _build pcre.cma netsys.cma equeue.cma netstring.cma netclient.cma util.cmo pffpsf.cmo www.cmo scan.cmo xmap.cmo print.cmo opt.cmo reddit.cmo
