open Core
open Scanf

let k = scanf "%d" Fn.id

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0

  let bits len = 0 -- (1 lsl len - 1)
  let by_bits x = map (fun bits -> (1 -- x) |> filter (on bits) |> map (fun x -> x - 1) |> to_array)
  let start x = by_bits x (bits x)
end
let ans =
  Bit_all.start 10
  |> Iter.map (fun arr -> Array.rev arr |> Array.fold ~init:0L ~f:Int64.(fun acc x -> acc * 10L + of_int x))
  |> Iter.sort ~cmp:Int64.compare
  |> Iter.drop 2
  |> Iter.drop (k - 1)
  |> Iter.head_exn

let () = printf "%Ld\n" ans
