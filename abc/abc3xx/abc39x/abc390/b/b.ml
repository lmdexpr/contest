open Core
open Scanf

let rec gcd =function 
  | 0, b -> b
  | a, 0 -> a
  | a, b -> gcd (if a < b then a, (b % a) else (a % b), b)
let gcd = Tuple2.curry gcd

module Ratio = struct
  type t = { num : int; den : int }

  let reduce { num; den } =
    let g = gcd num den in
    { num = num / g; den = den / g }

  let (/) num den = reduce { num; den }

  let (=) { num = num1; den = den1 } { num = num2; den = den2 } =
    num1 = num2 && den1 = den2
end

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let yes =
  Iter.(0 -- (n - 2))
  |> Iter.map Ratio.(fun i -> a.(i + 1) / a.(i))
  |> Iter.uniq ~eq:Ratio.(=)
  |> Iter.length
  |> (=) 1

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
