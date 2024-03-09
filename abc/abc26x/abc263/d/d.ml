open Core
open Scanf

let n, l, r = scanf "%d %d %d" Tuple3.create

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let dp = Array.init (n+1) ~f:(fun _ -> Array.init 3 ~f:(const 0)) 
let () =
  for i = 1 to n do
    let pred = dp.(i-1) in
    dp.(i).(0) <- pred.(0) + l;
    dp.(i).(1) <- min pred.(0) pred.(1) + a.(i-1);
    dp.(i).(2) <- min pred.(0) (min pred.(1) pred.(2)) + r
  done

let () = printf "%d\n%!" (min dp.(n).(0) @@ min dp.(n).(1) dp.(n).(2))
