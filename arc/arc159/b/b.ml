open Core
open Scanf

open Int64
    
let rec gcd = function
  | 0L, b -> b
  | a, 0L -> a
  | a, b -> gcd @@ if a < b then a, b % a else a % b, b
let gcd = Tuple2.curry gcd

let divisors x =
  let rec loop acc i =
    if i * i > x then acc
    else if x % i <> 0L then loop acc (i + 1L)
    else
      let acc = Iter.snoc acc i in
      let acc = if i = x / i then acc else Iter.snoc acc (x / i) in
      loop acc (i + 1L)
  in
  loop Iter.empty 1L |> Iter.filter ((<>) 1L)

let rec solve ?(acc=0L) a b =
  if a < 1L || b < 1L then acc
  else if a > b then solve ~acc b a
  else
    let g = gcd a b in
    let a = a / g and b = b / g in
    let t = 
      divisors (b - a)
      |> Iter.map (fun d -> a % d)
      |> Iter.min ~lt:(<)
      |> Option.value ~default:a
    in
    solve ~acc:(acc + t) (a - t) (b - t)

let a, b = scanf "%Ld %Ld" Tuple2.create
let ans  = solve a b
let ()   = printf "%Ld\n%!" ans
