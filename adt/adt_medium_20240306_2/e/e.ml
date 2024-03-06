open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let score i j =
  let dx = [-1; 0; 1; 0] in
  let dy = [0; -1; 0; 1] in
  List.zip_exn dx dy
  |> List.count ~f:(fun (dx, dy) ->
    let ni = i + dx in
    let nj = j + dy in
    0 <= ni && ni < h && 0 <= nj && nj < w && Char.(s.(ni).(nj) = '#')
  )

let i, j = 
  Iter.(
    (0 -- (h -1)) * (0 -- (w - 1))
  )
  |> Iter.filter_map (fun (i, j) ->
    Option.some_if Char.(s.(i).(j) = '.') (i, j)
  )
  |> Iter.find_pred_exn (fun (i, j) -> score i j > 1)

let i, j = i + 1, j + 1

let () = printf "%d %d\n%!" i j
