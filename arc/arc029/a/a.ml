open Core
open Scanf

let n = scanf "%d" ident

let t = Array.init n ~f:(fun _ -> scanf " %d" ident)

let ans =
  Iter.(0 -- (1 lsl n - 1))
  |> Iter.map (fun x ->
        Iter.(0 -- (n - 1))
        |> Iter.fold  (fun (a, b) i -> if x land (1 lsl i) <> 0 then t.(i) + a, b else a, t.(i) + b) (0, 0)
        |> Tuple2.uncurry max
    )
  |> Iter.min_exn ~lt:(<)

let () = printf "%d\n%!" ans
