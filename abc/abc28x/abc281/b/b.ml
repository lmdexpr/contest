open Core

let s = Scanf.scanf "%s" @@ fun s-> String.to_array s |> Array.map ~f:Char.to_int

let in_range lc uc c = Char.to_int lc <= c && c <= Char.to_int uc

let satisfied =
  Array.length s = 8
  && in_range 'A' 'Z' s.(0)
  && in_range 'A' 'Z' s.(7)
  && s.(1) <> Char.to_int '0'
  && (Array.slice s 1 7 |> Array.for_all ~f:(in_range '0' '9'))

let () =
  print_endline @@ if satisfied then "Yes" else "No"
