(* https://atcoder.jp/contests/pakencamp-2019-day3/tasks/pakencamp_2019_day3_d *)
open Core
open Scanf

let n = scanf "%d" ident

let c_to_i = function
  | 'R' -> 0
  | 'B' -> 1
  | 'W' -> 2
  | _   -> 3 
let flag = Array.init n ~f:(fun _ -> Array.init 4 ~f:(const 0))
let () =
  for _ = 0 to 4 do
    let s = scanf " %s" ident in
    for j = 0 to n-1 do
      let c = c_to_i s.[j] in
      flag.(j).(c) <- flag.(j).(c) + 1
    done
  done

let dp = Array.init (n+1) ~f:(fun _ -> Array.init 3 ~f:(const 5000))
let () =
  dp.(0) <- [| 0; 0; 0 |];
  for i = 0 to n - 1 do
    for j = 0 to 2 do 
      let j1 = (j + 1) % 3 in
      let j2 = (j + 2) % 3 in
      dp.(i+1).(j) <-
        min dp.(i+1).(j) @@
        min
          (dp.(i).(j1) + 5 - flag.(i).(j1))
          (dp.(i).(j2) + 5 - flag.(i).(j2))
    done
  done

let ans = dp.(n) |> Array.min_elt ~compare |> Option.value ~default:5000

let () = printf "%d\n%!" ans
