open Core
open Scanf

let n = scanf "%d" Fn.id
let h = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let h i j = abs (h.(i) - h.(j))

let dp = Array.create ~len:n Int.max_value

let chmin i v = dp.(i) <- min dp.(i) v

let () =
  dp.(0) <- 0;
  dp.(1) <- h 1 0

let () =
  for i = 2 to n - 1 do
    chmin i @@ dp.(i - 1) + h i (i - 1);
    chmin i @@ dp.(i - 2) + h i (i - 2);
  done

let ans = dp.(n - 1)

let () = printf "%d\n%!" ans
