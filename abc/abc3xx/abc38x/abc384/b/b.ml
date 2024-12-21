open Core
open Scanf

let n, r = scanf "%d %d" Tuple2.create

let ans = ref r
let () =
  for _ = 0 to n - 1 do
    match scanf " %d %d" Tuple2.create with
    | 1, a when 1600 <= !ans && !ans <= 2799 -> ans := !ans + a
    | 2, a when 1200 <= !ans && !ans <= 2399 -> ans := !ans + a
    | _ -> ()
  done

let ans = !ans

let () = printf "%d\n%!" ans
