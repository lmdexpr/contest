open Core
open Scanf

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0
 
  let bits len = 0 -- (1 lsl len - 1)
  let by_bits x = map (fun bits -> (1 -- x) |> filter (on bits) |> map (fun x -> x - 1) |> to_array)
  let start x = by_bits x (bits x)
end

let n = scanf " %d" Fn.id
let k = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let enumerate a =
  Bit_all.start Array.(length a)
  |> Iter.map Array.(fold ~init:0 ~f:(fun acc i -> acc + a.(i)))
  |> Iter.to_array

let l, r = Array.partitioni_tf a ~f:(fun i _ -> i < n / 2)

let l = enumerate l
let r = enumerate r

let yes =
  Array.sort l ~compare;
  Array.exists r ~f:(fun r ->
    Array.binary_search l ~compare `First_equal_to (k - r) |> Option.is_some
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
