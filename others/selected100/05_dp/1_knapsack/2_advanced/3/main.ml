(* https://atcoder.jp/contests/joi2013yo/tasks/joi2013yo_d *)
open Core
open Scanf

let d, n = scanf "%d %d" Tuple2.create

let t = Array.init (d + 1) ~f:(const 0)
let () =
  for i = 1 to d do
    scanf " %d" (Array.set t i);
  done

let a = Array.init (n + 1) ~f:(const 0)
let b = Array.init (n + 1) ~f:(const 0)
let c = Array.init (n + 1) ~f:(const 0)
let () =
  for i = 1 to n do
    scanf " %d" (Array.set a i);
    scanf " %d" (Array.set b i);
    scanf " %d" (Array.set c i);
  done

let dp = Array.init (d + 1) ~f:(fun _ -> Array.init (n + 1) ~f:(const 0))
let () =
  for i = 2 to d do
    for j = 1 to n do
      if a.(j) <= t.(i) && t.(i) <= b.(j) then
        for k = 1 to n do
          if a.(k) <= t.(i - 1) && t.(i - 1) <= b.(k) then
            dp.(i).(j) <- max dp.(i).(j) (dp.(i - 1).(k) + abs (c.(j) - c.(k)))
        done
    done
  done

let ans = Array.max_elt ~compare dp.(d) |> Option.value ~default:0

let () = printf "%d\n%!" ans
