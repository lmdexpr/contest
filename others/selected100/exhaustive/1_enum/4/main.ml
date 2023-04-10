(* https://atcoder.jp/contests/pakencamp-2019-day3/tasks/pakencamp_2019_day3_c *)

open Core
open Scanf

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

let n, m = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> Array.init m ~f:(fun _ -> scanf " %Ld" ident))
let a = Array.transpose_exn a

let ans =
  Iter.( (1 -- m) * (1 -- m) )
  |> Iter.map (fun (t1, t2) -> t1 - 1, t2 - 1)
  |> Iter.filter (Tuple2.uncurry (<>))
  |> Iter.map Int64.(fun (t1, t2) ->
      Array.zip_exn a.(t1) a.(t2)
      |> Array.map ~f:(Tuple2.uncurry Int64.max)
      |> Array.fold ~init:0L ~f:(+)
    )
  |> Iter.max_exn ~lt:Int64.(<)

let () = printf "%Ld\n%!" ans
