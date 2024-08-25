open Core

let s, t = Scanf.scanf "%s %s" Tuple2.create

let () =
  print_endline @@ if String.is_substring s ~substring:t then "Yes" else "No"
