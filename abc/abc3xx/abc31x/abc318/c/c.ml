open Core
open Scanf

let n, d, p = scanf "%d %d %Ld" Tuple3.create

let f  = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)
let () = Array.sort f ~compare:Int64.descending

let rec interval_sum ?(acc = 0L) start stop =
  if start >= min n stop then acc
  else interval_sum ~acc:Int64.(acc + f.(start)) (start + 1) stop

let rec solve ds acc i =
  if i >= n - d then
    if Int64.(ds < p) then
      Int64.(acc + interval_sum i n)
    else
      Int64.(acc + p)
  else
    if Int64.(ds < p) then
      solve Int64.(ds - f.(i) + f.(Int.(i + d))) Int64.(acc + f.(i)) (i + 1)
    else
      solve (interval_sum (i + d) (i + d + d)) Int64.(acc + p) (i + d)

let ans = solve (interval_sum 0 d) 0L 0

let () = printf "%Ld\n%!" ans
