open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.create ~len:n 0
let b = Array.create ~len:n 0
let () =
  for i = 0 to n - 1 do
    scanf " %d %d" (fun x y -> a.(i) <- x; b.(i) <- y - x)
  done

let ans = 
  Array.sum (module Int) ~f:Fn.id a +
  Option.value_exn (Array.max_elt ~compare b)

let () = printf "%d\n%!" ans
