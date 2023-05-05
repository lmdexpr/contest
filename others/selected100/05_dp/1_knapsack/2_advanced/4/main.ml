(* https://atcoder.jp/contests/joi2015yo/tasks/joi2015yo_d *)
open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let d = Array.init n ~f:(fun _ -> scanf " %d" ident)
let c = Array.init m ~f:(fun _ -> scanf " %d" ident)

let (.!()<-) a i v = a.(i) <- min a.(i) v

let dp = Array.init (n + 1) ~f:(fun _ -> Array.create ~len:(m + 1) 2_000_000_000)
let () =
  dp.(0).(0) <- 0;
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      dp.( i ).!(j+1) <- dp.(i).(j);
      dp.(i+1).!(j+1) <- dp.(i).(j) + d.(i) * c.(j)
    done
  done

let ans = Array.min_elt dp.(n) ~compare:Int.compare |> Option.value ~default:0

let () = printf "%d\n%!" ans
