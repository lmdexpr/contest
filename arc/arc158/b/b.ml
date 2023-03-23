open Core
open Scanf

let n = scanf "%d" ident

let x = Array.init n ~f:(fun _ -> scanf " %d" ident)
let () = Array.sort x ~compare:(fun a b -> Float.compare (1. /. float a) (1. /. float b))

let x = Int.Set.of_array [| 0; 1; 2; n-3; n-2; n-1 |] |> Int.Set.to_array |> Array.map ~f:(Array.get x)
let n = Array.length x

let ans =
  Iter.(0 -- (1 lsl n - 1))
  |> Iter.map (fun bits ->
      Iter.(0 -- (n - 1))
      |> Iter.filter (fun i -> bits land (1 lsl i) <> 0)
      |> Iter.map (Array.get x)
      |> Iter.to_array
    )
  |> Iter.filter_map (function
      | [| xi; xj; xk |] -> Some (float (xi + xj + xk) /. float (xi * xj * xk))
      | _                -> None
    )

let min = Iter.min_exn ~lt:Float.(<) ans
let max = Iter.max_exn ~lt:Float.(<) ans

let () = printf "%.15f\n%.15f\n%!" min max
