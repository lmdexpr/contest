open Core
open Scanf

let a = Array.make_matrix ~dimx:9 ~dimy:9 0
let () =
  scanf "%d" (fun x -> a.(0).(0) <- x);
  for i = 0 to 8 do
    for j = 0 to 8 do
      if i <> 0 || j <> 0 then
        scanf " %d" (fun x -> a.(i).(j) <- x)
    done
  done

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let check a =
  let s = Int.Set.of_array a in
  Set.length s = 9

let row = Array.for_all a ~f:check
let col = Array.transpose_exn a |> Array.for_all ~f:check
let box =
  Iter.((0 -- 2) * (0 -- 2)) |> Iter.for_all (fun (i, j) ->
    Iter.((0 -- 2) * (0 -- 2))
    |> Iter.fold
      (fun s (k, l) -> Set.add s a.(i * 3 + k).(j * 3 + l))
      Int.Set.empty
    |> fun s -> Set.length s = 9
  )

let yes = row && col && box
let ans = if yes then "Yes" else "No"
let ()  = printf "%s\n%!" ans
