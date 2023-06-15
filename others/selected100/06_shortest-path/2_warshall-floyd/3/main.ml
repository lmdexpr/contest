(* https://atcoder.jp/contests/abc079/tasks/abc079_d *)
open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let scan_matrix n m = Array.init n ~f:(fun _ -> Array.init m ~f:(fun _ -> scanf " %d" ident))

let c = scan_matrix 10 10
let a = scan_matrix h  w

let inf = 10_000
let _warshall_floyd =
  for k = 0 to 9 do
    for i = 0 to 9 do
      for j = 0 to 9 do
        if c.(i).(k) < inf && c.(k).(j) < inf then
          c.(i).(j) <- min c.(i).(j) @@ c.(i).(k) + c.(k).(j)
      done
    done
  done

let ans =
  Array.fold a ~init:0 ~f:(fun acc ->
      Array.fold ~init:acc ~f:(fun acc -> function
          | -1 -> acc
          | i  -> acc + c.(i).(1)
        )
    )

let () = printf "%d\n%!" ans
