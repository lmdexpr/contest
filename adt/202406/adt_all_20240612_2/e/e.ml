open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create
let a = Array.init h ~f:(fun _ -> Array.init w ~f:(fun _ -> scanf " %d" Fn.id))

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0
  let bits len = 0 -- (1 lsl len - 1)
end

let l = h + w - 2

let solve bits =
  let rec loop (i, j) set k =
    if k > l then set
    else
      loop 
        (if Bit_all.on bits k then i + 1, j else i, j + 1)
        (Set.add set a.(i).(j))
        (k + 1)
  in
  loop (0, 0) Int.Set.empty 1
  |> Set.length = l

let ans =
  Bit_all.bits l
  |> Iter.filter (fun bits -> Int.popcount bits = h - 1)
  |> Iter.filter solve
  |> Iter.length

let () = printf "%d\n" ans
