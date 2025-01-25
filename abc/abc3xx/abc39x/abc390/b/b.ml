open Core
open Scanf

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

type ratio = { num : int; den : int }

let compare r1 r2 =
  let open Int64 in
  compare (of_int r1.num * of_int r2.den) (of_int r2.num * of_int r1.den)

let yes =
  Iter.(0 -- (n - 2))
  |> Iter.map (fun i -> { num = a.(i + 1); den = a.(i) })
  |> Iter.uniq ~eq:(fun r1 r2 -> compare r1 r2 = 0)
  |> Iter.length
  |> (=) 1

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
