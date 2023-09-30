open Core
open Scanf

let p = Array.init 3 ~f:(fun _ ->
  Array.init 4 ~f:(fun _ -> scanf " %s" String.to_array |> Array.map ~f:Char.(fun c -> c = '#'))
)

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let yes =
  Iter.(
    (((0 -- 4) * (0 -- 4)) * (0 --3)) *
    (((0 -- 4) * (0 -- 4)) * (0 --3)) *
    (((0 -- 4) * (0 -- 4)) * (0 --3))
  )
  |> Iter.exists (fun ((p0, p1), p2) ->
    let a = Array.make_matrix ~dimx:8 ~dimy:8 0 in
    let update ((x, y), z) =
      Array.iteri ~f:(fun i -> Array.iteri ~f:(fun j c ->
        let i, j = match z with
          | 0 -> i,     j
          | 1 -> 3 - j, i
          | 2 -> 3 - i, 3 - j
          | _ -> j,     3 - i
        in
        let x = x + i and y = y + j in
        if c then a.(x).(y) <- a.(x).(y) + 1
      ))
    in
    update p0 p.(0); update p1 p.(1); update p2 p.(2);
    let is_fill a = 
      a.(0) = 0 && a.(1) = 0 &&
      a.(2) = 1 && a.(3) = 1 && a.(4) = 1 && a.(5) = 1 &&
      a.(6) = 0 && a.(7) = 0
    in
    Array.for_all a.(0) ~f:(fun a -> a = 0) &&
    Array.for_all a.(1) ~f:(fun a -> a = 0) &&
    is_fill a.(2) &&
    is_fill a.(3) &&
    is_fill a.(4) &&
    is_fill a.(5) &&
    Array.for_all a.(6) ~f:(fun a -> a = 0) &&
    Array.for_all a.(7) ~f:(fun a -> a = 0)
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
