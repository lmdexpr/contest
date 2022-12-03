open Core

let s, t = Scanf.scanf "%s %s" Tuple2.create

let length = String.length s

let rec solve i =
  if i = length || Char.(s.[i] <> t.[i]) then i + 1
  else
    solve @@ i + 1

let () = printf "%d\n%!" @@ solve 0
