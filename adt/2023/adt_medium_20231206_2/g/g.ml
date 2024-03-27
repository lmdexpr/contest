open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.make_matrix ~dimx:(2 * n + 1) ~dimy:(2 * n + 1) 0
let () =
  for i = 1 to 2 * n do
    for j = 1 to 2 * n do
      a.(i).(j) <-
        if i = j then 0
        else if i < j then scanf "%d" Fn.id
        else
          a.(j).(i)
    done
  done

let ans = 0

let () = printf "%d\n%!" ans
