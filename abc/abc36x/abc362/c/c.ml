open Core
open Scanf

let n = scanf "%d" Fn.id

let l = Array.create ~len:n 0
let r = Array.create ~len:n 0

let () =
  for i = 0 to n - 1 do
    let li, ri = scanf " %d %d" Tuple2.create in
    l.(i) <- li; r.(i) <- ri
  done

let sum_l = Array.sum (module Int) ~f:Fn.id l
let sum_r = Array.sum (module Int) ~f:Fn.id r
let () =
  if 0 < sum_l || sum_r < 0 then (
    printf "No\n";
    exit 0
  )

let ans =
  let sum = ref sum_l in
  Array.init n ~f:(fun i ->
    let d = min (r.(i) - l.(i)) (- !sum) in
    sum := !sum + d;
    d + l.(i)
  )

let () =
  printf "Yes\n";
  Array.iter ~f:(printf "%d ") ans;
  printf "\n"
