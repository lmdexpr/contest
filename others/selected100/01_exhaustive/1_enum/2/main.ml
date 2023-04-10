(* https://atcoder.jp/contests/abc106/tasks/abc106_b *)

open Core
open Scanf

let n = scanf "%d" ident
 
let divisors x =
  let rec loop acc i =
    if i * i > x then acc
    else if x % i <> 0 then loop acc (i + 1)
    else
      let acc = Iter.snoc acc i in
      let acc = if i = x / i then acc else Iter.snoc acc (x / i) in
      loop acc (i + 1)
  in
  loop Iter.empty 1

let ans =
  Iter.(1 -- n)
  |> Iter.filter (fun x -> x % 2 = 1)
  |> Iter.filter (fun x -> Iter.length (divisors x) = 8)
  |> Iter.length

let () = printf "%d\n%!" ans
