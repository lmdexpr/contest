open Core

let n = Scanf.scanf "%d" ident

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)
let () = Array.sort a ~compare

let b = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)
let () = Array.sort b ~compare

let () =
  Array.zip_exn a b
  |> Array.fold ~init:0 ~f:(fun acc (a, b) -> acc + abs (a - b))
  |> printf "%d\n%!"
