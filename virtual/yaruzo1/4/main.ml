open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %Ld" ident)

let dp = Array.make_matrix ~dimx:(n+1) ~dimy:2 0L
let () =
  dp.(0).(0) <- 0L;
  dp.(0).(1) <- -1_000_000_000_000_000_000L;
  for i = 0 to n - 1 do
    dp.(i+1).(0) <- Int64.(max (dp.(i).(0) + a.(i)) (dp.(i).(1) - a.(i)));
    dp.(i+1).(1) <- Int64.(max (dp.(i).(0) - a.(i)) (dp.(i).(1) + a.(i)));
  done

let ans = dp.(n).(0)

let () = printf "%Ld\n%!" ans
