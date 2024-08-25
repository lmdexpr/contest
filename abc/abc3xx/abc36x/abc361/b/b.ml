open Core
open Scanf

let cube a b c d e f = (a, b, c), (d, e, f)

let (a, b, c), (d, e, f) = scanf  "%d %d %d %d %d %d" cube
let (g, h, i), (j, k, l) = scanf " %d %d %d %d %d %d" cube

let yes =
  let max_1 = [| max a d; max b e; max c f |] in
  let min_1 = [| min a d; min b e; min c f |] in
  let max_2 = [| max g j; max h k; max i l |] in
  let min_2 = [| min g j; min h k; min i l |] in
  Iter.(0 -- 2)
  |> Iter.for_all (fun i ->
    max_1.(i) > min_2.(i) && min_1.(i) < max_2.(i)
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
