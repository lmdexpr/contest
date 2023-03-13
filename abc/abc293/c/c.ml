open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let a = Array.init h ~f:(fun _ -> Array.init w ~f:(fun _ -> scanf " %d" ident))

let ans =
  Iter.(0 -- (1 lsl (h + w - 2) - 1))
  |> Iter.filter (fun x -> Int.popcount x = h - 1)
  |> Iter.map (fun x ->
      Iter.(0 -- (h + w - 2 - 1))
      |> Iter.map  (fun i -> if x land (1 lsl i) <> 0 then 1, 0 else 0, 1)
      |> Iter.scan (fun (x, y) (dx, dy) -> x + dx, y + dy) (0, 0)
      |> Iter.map  (fun (x, y) -> a.(x).(y))
      |> Iter.fold Int.Set.add Int.Set.empty
    )
  |> Iter.filter (fun set -> Int.Set.length set = h + w - 1)
  |> Iter.length

let () = printf "%d\n%!" ans
