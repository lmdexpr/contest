open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let ab = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)
let () = Array.sort ab ~compare:(fun (a, _) (b, _) -> Int.compare a b)

let day1 = Array.sum (module Int) ab ~f:Tuple2.get2

let rec loop ?(medicine=day1) ?(day=1) i =
  if medicine <= k then day
  else
    let a, b = ab.(i) in
    let medicine = medicine - b in
    loop ~medicine ~day:(a+1) (i + 1)

let () = printf "%d\n" (loop 0)
