open Core
open Scanf

let n = scanf "%d" Fn.id
let k = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let sum = Array.fold k ~init:0L ~f:Int64.(+)

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0
 
  let bits len = 0 -- (1 lsl len - 1)
  let by_bits x = map (fun bits -> (1 -- x) |> filter (on bits) |> map (fun x -> x - 1) |> to_array)
  let start x = by_bits x (bits x)
end

let ans =
  Bit_all.start n
  |> Iter.map (fun idxes ->
    Array.fold idxes ~init:0L ~f:Int64.(fun acc i -> acc + k.(i))
  )
  |> Iter.map Int64.(fun a -> max a (sum - a))
  |> Iter.min_exn ~lt:Int64.(<)

let () = printf "%Ld\n%!" ans
