(* unsolved *)
open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let e = Array.create ~len:(n+1) []
let e =
  for i = 0 to n - 1 do
    e.(a.(i)) <- i :: e.(a.(i))
  done;
  Array.map e ~f:Array.of_list_rev
  
let q = scanf " %d" Fn.id
let () = 
  for _ = 1 to q do
    let l, r, x = scanf " %d %d %d" (fun l r x -> l - 1, r - 1, x) in
    let r = Array.binary_search e.(x) ~compare `Last_less_than_or_equal_to r in
    let r = Option.value ~default:(Array.length e.(x) - 1) r in
    let l = Array.binary_search e.(x) ~compare `First_greater_than_or_equal_to l in
    let l = Option.value ~default:0 l in
    let ans = r - l in
    printf "%d\n" ans
  done
