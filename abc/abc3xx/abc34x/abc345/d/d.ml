open Core
open Scanf

let n, h, w = scanf "%d %d %d" Tuple3.create
let ab = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0

  let bits len = 0 -- (1 lsl len - 1)
  let by_bits x = map (fun bits -> (1 -- x) |> filter (on bits) |> map (fun x -> x - 1) |> to_array)
  let start x = by_bits x (bits x)
end

let yes =
  Bit_all.start n
  |> Iter.find_pred (fun _bits ->
    false
  )
  |> Option.is_some

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
