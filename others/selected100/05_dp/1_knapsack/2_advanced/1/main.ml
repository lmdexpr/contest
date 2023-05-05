(* https://atcoder.jp/contests/joi2011yo/tasks/joi2011yo_d *)
open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let dp = Array.init (n+1) ~f:(fun _ -> Array.create ~len:21 0L)
let () =
  dp.(0).(a.(0)) <- 1L;
  for i = 1 to n - 2 do
    let a = a.(i) in
    for j = 0 to 20 do
      if j - a >= 0 then
        dp.(i).(j) <- Int64.(+) dp.(i).(j) dp.(i-1).(j-a);
      if j + a <= 20 then
        dp.(i).(j) <- Int64.(+) dp.(i).(j) dp.(i-1).(j+a)
    done
  done

let ans = dp.(n - 2).(Array.last a)

let () = printf "%Ld\n%!" ans
