open Core
open Scanf

let n, m, t = scanf "%d %d %d" Tuple3.create

let a = Array.init (n - 1) ~f:(fun _ -> scanf " %d" Fn.id)
let b = Array.create ~len:n 0
let () =
  for _ = 1 to m do
    let x, y = scanf " %d %d" Tuple2.create in
    b.(x - 1) <- y
  done

let yes =
  Array.folding_mapi a ~init:t ~f:(fun i t a -> t - a + b.(i+1), t - a > 0)
  |> Array.for_all ~f:Fn.id

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
