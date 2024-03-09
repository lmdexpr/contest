open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let s = Array.init h ~f:(fun _ -> 
  scanf " %s" String.to_array |> Array.map ~f:Char.(fun c -> c = '#')
)

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let dfs i j =
  let rec loop = function
  | [] -> ()
  | (i, j) :: rest when not s.(i).(j) -> loop rest
  | (i, j) :: rest ->
    s.(i).(j) <- false;
    Iter.((-1 -- 1) * (-1 -- 1))
    |> Iter.filter (fun (di, dj) -> not (di = 0 && dj = 0))
    |> Iter.map    (fun (di, dj) -> i + di, j + dj)
    |> Iter.filter (fun (i, j) -> 0 <= i && i < h && 0 <= j && j < w && s.(i).(j))
    |> Iter.fold (Fn.flip List.cons) rest
    |> loop
  in
  loop [(i, j)]

let ans =
  Iter.((0 -- (h - 1)) * (0 -- (w - 1)))
  |> Iter.fold (fun acc (i, j) ->
    if not s.(i).(j) then acc
    else
      (dfs i j; acc + 1)
  ) 0

let () = printf "%d\n%!" ans
