open Core

let _n = Scanf.scanf "%d" ident
let s = Scanf.scanf " %s" ident

let () = printf "%s\n%!" @@ String.substr_replace_all ~pattern:"na" ~with_:"nya" s
