open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.create ~len:(n + 1) (-1, -1)
let () =
  for i = 1 to 2 * n do
    let ai = scanf " %d" Fn.id in
    a.(ai) <-
      match a.(ai) with
      | (-1, _) -> (i, -1)
      | (j, _)  -> (j, i)
  done

let ans =
  Array.count a ~f:(fun (i, j) -> j - i = 2)

let () = printf "%d\n%!" ans
