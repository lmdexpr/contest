open Core
open Scanf

let n, m, k = scanf "%d %d %d" Tuple3.create

let a =
  Array.init m ~f:(fun _ ->
    let c = scanf " %d" Fn.id in
    let a = Array.init c ~f:(fun _ -> scanf " %d" Fn.id) in
    let r = scanf " %c" Fn.id in
    Int.Set.of_array a, Char.equal r 'o'
  )

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0

  let bits len = 0 -- (1 lsl len - 1)
  let by_bits x = map (fun bits -> (1 -- x) |> filter (on bits) |> to_array)
  let start x = by_bits x (bits x)
end

let ans =
  Bit_all.start n
  |> Iter.filter_count (fun bits ->
    Array.for_all a ~f:(fun (a, r) ->
      let satisfy = Array.count bits ~f:(fun i -> Set.mem a i) >= k in
      Bool.(satisfy = r)
    )
  )

let () = printf "%d\n%!" ans
