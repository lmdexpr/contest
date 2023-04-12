(* https://atcoder.jp/contests/joi2007ho/tasks/joi2007ho_c *)

open Core
open Scanf

let n = scanf "%d" ident

let pillars = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)
let compare = Tuple2.compare ~cmp1:compare ~cmp2:compare
let () = Array.sort pillars ~compare

let ans =
  Iter.(0 -- (n - 1))
  |> Iter.flat_map (fun p -> Iter.(0 -- (n - 1)) |> Iter.map (fun q -> p, q))
  |> Iter.filter (Tuple2.uncurry (<))
  |> Fn.flip Iter.fold 0
    (fun best (i, j) ->
       let xi, yi = pillars.(i) and xj, yj = pillars.(j) in
       if xi = xj && yi = yj then best
       else
         let x = xi - xj and y = yi - yj in
         let p = xj + y, yj - x in
         let q = xi + y, yi - x in
         let search = Array.binary_search pillars ~compare `First_equal_to in
         match search p, search q with
         | Some _, Some _ -> max best (x * x + y * y)
         | _              -> best
    )

let () = printf "%d\n%!" ans
