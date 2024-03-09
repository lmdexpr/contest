open Core
open Scanf

let ha, wa = scanf "%d %d" Tuple2.create
let a = Array.init ha ~f:(fun _ -> Array.init wa ~f:(fun _ -> scanf " %d" ident))

let hb, wb = scanf " %d %d" Tuple2.create
let b = Array.init hb ~f:(fun _ -> Array.init wb ~f:(fun _ -> scanf " %d" ident))

module Iter = struct
  include Iter
  let zip x y = flat_map (fun x -> map (fun y -> x, y) y) x
  let( * ) = zip
end

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0

  let bits len = 0 -- (1 lsl len - 1)
  let by_bits x = map (fun bits -> (1 -- x) |> filter (on bits) |> map (fun x -> x - 1) |> to_array)
  let start x = by_bits x (bits x)
end

let yes =
  Iter.(
    Bit_all.start ha * Bit_all.start wa
  )
  |> Iter.filter (fun (i, j) -> Array.length i = hb && Array.length j = wb)
  |> Iter.exists (fun (i, j) ->
      Iter.(
        (0 -- (hb - 1)) * (0 -- (wb - 1))
      )
      |> Iter.for_all (fun (k, l) -> a.(i.(k)).(j.(l)) = b.(k).(l))
    )

let () = if yes then printf "Yes\n%!" else printf "No\n%!"
