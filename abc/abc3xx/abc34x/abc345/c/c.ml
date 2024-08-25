open Core
open Scanf

let s = scanf "%s" Fn.id
let n = String.length s |> Int64.of_int

let count = Array.create ~len:26 0L
let () =
  String.iter s ~f:(fun c ->
    let i = Char.to_int c - Char.to_int 'a' in
    count.(i) <- Int64.(count.(i) + 1L)
  )

let ans = Int64.(
  Array.fold count ~init:(n * n) ~f:(fun acc x -> acc - x * x) / 2L
  + if Array.exists ~f:(fun c -> c > 1L) count then 1L else 0L
)

let () = printf "%Ld\n%!" ans
