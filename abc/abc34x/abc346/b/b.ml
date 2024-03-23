open Core
open Scanf

let w, b = scanf "%d %d" Tuple2.create

let s =
  List.init 30 ~f:(const @@ String.to_list "wbwbwwbwbwbw")
  |> List.concat

let rec solve w b = function
  | 'w' :: t -> if w <= 0 then b = 0 else solve (w - 1) b t
  | 'b' :: t -> if b <= 0 then w = 0 else solve w (b - 1) t
  | _        -> true

let yes =
  Iter.(0 -- 24)
  |> Iter.map (fun i -> List.drop s i)
  |> Iter.exists (solve w b)

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
