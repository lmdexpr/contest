open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let h = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let h i j = abs (h.(i) - h.(j))

let dp = Array.create ~len:n Int.max_value

let chmin i v = dp.(i) <- min dp.(i) v

let () = dp.(0) <- 0

let () =
  for i = 0 to n - 1 do
    for j = 1 to k do
      if i + j < n then
        chmin (i + j) @@ dp.(i) + h i (i + j)
    done
  done

let ans = dp.(n - 1)

let () = printf "%d\n%!" ans
