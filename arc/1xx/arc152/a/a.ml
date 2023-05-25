open Core
open Scanf

let n, l = scanf "%d %d" Tuple2.create

let a = List.init n ~f:(fun _ -> scanf " %d" ident)

let satisfied =
  Iter.of_list a
  |> Iter.drop_while ((<>) 2) |> Iter.drop 1
  |> Iter.map ((+) 1)
  |> Iter.fold (-) l
  |> fun rest -> rest >= 2

let () = printf "%s\n%!" @@ if satisfied then "Yes" else "No"
