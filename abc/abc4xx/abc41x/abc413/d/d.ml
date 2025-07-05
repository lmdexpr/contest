open Core
open Scanf

let rec gcd =function 
  | 0, b -> b
  | a, 0 -> a
  | a, b -> gcd (if a < b then a, (b % a) else (a % b), b)
let gcd = Tuple2.curry gcd

module Ratio = struct
  type t = { sign : int; num : int; den : int }

  let reduce { sign; num; den } =
    let g = gcd num den in
    { sign; num = num / g; den = den / g }

  let (/) num den = 
    let sign x = 
      if x < 0 then -1
      else if x > 0 then 1
      else 0
    in
    reduce {
      sign = sign num * sign den;
      num = abs num;
      den = abs den;
    }

  let (=)
    { sign = sign1; num = num1; den = den1 }
    { sign = sign2; num = num2; den = den2 }
    =
    sign1 = sign2 && num1 = num2 && den1 = den2
end

let yes n a =
  Array.sort a ~compare:(fun x y -> compare (abs x) (abs y));
  Iter.(0 -- (n - 3)) |> Iter.for_all (fun i ->
    Ratio.(a.(i) / a.(i + 1) = a.(i + 1) / a.(i + 2))
  )

let t = scanf " %d" Fn.id

let () =
  for _ = 1 to t do
    let n = scanf " %d" Fn.id in
    let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id) in
    printf "%s\n%!" @@ if yes n a then "Yes" else "No"
  done
