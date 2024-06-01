open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let a = Array.init m ~f:(fun _ -> scanf " %d" Fn.id)

let () =
  for _ = 1 to n do
    for i = 0 to m - 1 do
      let x = scanf " %d" Fn.id in
      a.(i) <- a.(i) - x
    done
  done

let yes = Array.for_all a ~f:(fun x -> x <= 0)
let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
