open Core
open Scanf

let n = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let yes =
  Iter.(0 -- (n - 3))
  |> Iter.exists (fun i ->
    a.(i) = a.(i + 1) && a.(i + 1) = a.(i + 2)
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
