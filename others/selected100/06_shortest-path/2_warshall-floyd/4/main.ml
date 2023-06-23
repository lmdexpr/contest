(* https://atcoder.jp/contests/abc074/tasks/arc083_b *)
open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ -> scanf " %d" ident))

let inf = 10_000_000_000
let b = Array.init n ~f:(fun i -> Array.copy a.(i))
let () = for i = 0 to n - 1 do b.(i).(i) <- inf done

let _warshall_floyd =
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if b.(i).(k) + b.(k).(j) < a.(i).(j) then begin
          printf "-1\n%!";
          exit 0
        end
        else if b.(i).(k) + b.(k).(j) = a.(i).(j) then 
          b.(i).(j) <- inf
      done
    done
  done
  
let filter x = if x < inf then x else 0

let ans = Array.sum (module Int) b ~f:(Array.sum (module Int) ~f:filter)
let ans = ans / 2

let () = printf "%d\n%!" ans
