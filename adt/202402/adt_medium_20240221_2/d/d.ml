open Core
open Scanf

let n  = scanf "%d" Fn.id
let xy = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let ans =
  Iter.((0 -- (n - 1)) * (0 -- (n - 1)))
  |> Iter.map (fun (i, j) ->
    let xi, yi = xy.(i) in
    let xj, yj = xy.(j) in
    Float.sqrt (Float.of_int ((xi - xj) * (xi - xj) + (yi - yj) * (yi - yj)))
  )
  |> Iter.fold Float.max Float.min_value

let () = printf "%f\n%!" ans
