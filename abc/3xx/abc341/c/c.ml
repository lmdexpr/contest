open Core
open Scanf

let h, w, _n = scanf "%d %d %d" Tuple3.create
let t = scanf " %s" String.to_list
let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array |> Array.map ~f:Char.((=) '.'))

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end


let rec solve (i, j) = function
  | []       -> s.(i).(j)
  | 'L' :: t -> s.(i).(j) && solve (i, j - 1) t
  | 'R' :: t -> s.(i).(j) && solve (i, j + 1) t
  | 'U' :: t -> s.(i).(j) && solve (i - 1, j) t
  | 'D' :: t -> s.(i).(j) && solve (i + 1, j) t
  | _        -> false


let ans =
  Iter.((1 -- (h -2)) * (1 -- (w - 2)))
  |> Iter.filter_count (Fn.flip solve t)

let () = printf "%d\n%!" ans
