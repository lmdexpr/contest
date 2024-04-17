open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create
let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array |> Array.map ~f:Char.((=) '#'))

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let check (i, j) =
  let around =
    [ (-1, 0); (1, 0); (0, -1); (0, 1) ] 
    |> List.filter ~f:(fun (di, dj) ->
      let i, j = i + di, j + dj in
      0 <= i && i < h && 0 <= j && j < w && s.(i).(j)
    )
    |> List.length
  in
  not s.(i).(j) && around >= 2

let i, j = 
  Iter.((0 -- (h-1)) * (0 -- (w-1)))
  |> Iter.find_pred check
  |> Option.value_exn

let () = printf "%d %d\n%!" (i + 1) (j + 1)
