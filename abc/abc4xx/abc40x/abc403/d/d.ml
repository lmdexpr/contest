open Core
open Scanf

let n = scanf " %d" Fn.id
let d = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let count = Array.create ~len:1_000_001 0
let () =
  Array.iter a ~f:(fun x -> count.(x) <- count.(x) + 1)

let ans =
  if d = 0 then n - Array.count count ~f:(fun x -> x > 0)
  else
    Iter.(0 -- (d - 1))
    |> Iter.map (fun i ->
      Iter.int_range_by ~step:d i 1_000_000
      |> Iter.map (Array.get count)
      |> Iter.fold
        (fun (px, prev, now) x -> x, now, min (now + x) (prev + px))
        (0, 0, 0)
      |> Tuple3.get3
    )
    |> Iter.sum

let () = printf "%d\n%!" ans
