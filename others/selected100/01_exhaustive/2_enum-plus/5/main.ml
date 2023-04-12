(* https://atcoder.jp/contests/joi2008yo/tasks/joi2008yo_d *)
open Core
open Scanf

let compare = Tuple2.compare ~cmp1:compare ~cmp2:compare

let m = scanf "%d" ident
let star = Array.init m ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let n = scanf " %d" ident
let photo = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)
let () = Array.sort photo ~compare

let ans =
  Iter.of_array photo
  |> Iter.map (fun (x, y) ->
      let ox, oy = star.(0) in
      let dx, dy = x - ox, y - oy in
      dx, dy, Iter.of_array star |> Iter.map (fun (x, y) -> x + dx, y + dy)
    )
  |> Iter.filter (fun (_, _, stars) -> Iter.for_all (fun x -> Array.binary_search photo ~compare `First_equal_to x |> Option.is_some) stars)
  |> Iter.head

let (x, y, _) = Option.value_exn ans

let () = printf "%d %d\n%!" x y
