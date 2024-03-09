open Core
open Scanf

let n = scanf "%d" ident

let box = Array.init n ~f:(fun _ ->
  scanf " %d %d %d" @@ fun x y z ->
  match List.sort [x; y; z] ~compare with
  | [x; y; z] -> x, y, z
  | _         -> assert false
)
let () = Array.sort box ~compare:(Tuple3.compare ~cmp1:compare ~cmp2:compare ~cmp3:compare)

let yes =
  Iter.(0 -- (n - 2))
  |> Iter.fold (fun yes i ->
    let x1, y1, z1 = box.(i) in
    let x2, y2, z2 = box.(i + 1) in
    yes || (x1 < x2 && y1 < y2 && z1 < z2)
  ) false 

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
