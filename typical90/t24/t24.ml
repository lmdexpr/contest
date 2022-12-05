open Core

let n, k = Scanf.scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)
let b = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let count =
  Array.zip_exn a b
  |> Array.fold ~init:0 ~f:(fun acc (a, b) -> acc + abs (a - b))

let () =
  print_endline @@ if count <= k && (k - count) % 2 = 0 then "Yes" else "No"
