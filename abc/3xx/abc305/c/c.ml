open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let s = Array.init h ~f:(fun _ -> scanf " %s" ident)

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let i, j =
  Iter.(
    (0 -- (h - 1)) * (0 -- (w - 1))
  )
  |> Iter.filter Char.(fun (i, j) -> s.(i).[j] = '.')
  |> Iter.find_pred_exn (fun (i, j) ->
      Iter.of_array [| -1, 0; 1, 0; 0, -1; 0, 1 |]
      |> Iter.map (fun (di, dj) -> i + di, j + dj)
      |> Iter.filter (fun (i, j) -> 0 <= i && i < h && 0 <= j && j < w)
      |> Iter.filter_count Char.(fun (i, j) -> s.(i).[j] = '#')
      |> fun block -> block >= 2
    )
let i, j = i + 1, j + 1

let () = printf "%d %d\n%!" i j
