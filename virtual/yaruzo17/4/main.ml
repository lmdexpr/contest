open Core
open Scanf

let s = scanf "%s" Fn.id
let n = String.length s + 1

let a = Array.create ~len:n 0

let () =
  for i = 0 to n - 2 do
    if Char.(s.[i] = '<') then a.(i + 1) <- a.(i) + 1
  done;
  for i = n - 1 downto 1 do
    if Char.(s.[i - 1] = '>') then a.(i - 1) <- Int.max a.(i - 1) (a.(i) + 1)
  done

let ans = Array.sum (module Int) a ~f:Fn.id

let () = printf "%d\n%!" ans
