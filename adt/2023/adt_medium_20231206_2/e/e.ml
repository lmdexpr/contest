open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id)

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0

  let bits len = 0 -- (1 lsl len - 1)
  let by_bits x = map (fun bits -> (1 -- x) |> filter (on bits) |> map (fun x -> x - 1) |> to_array)
  let start x = by_bits x (bits x)
end

let ans =
  Bit_all.start n
  |> Iter.map (fun choice ->
    let chars = Array.create ~len:26 0 in
    Array.iter choice ~f:(fun i ->
      String.iter s.(i) ~f:(fun c ->
        let c = Char.to_int c - Char.to_int 'a' in
        chars.(c) <- chars.(c) + 1
      )
    );
    chars |> Array.filter ~f:(fun x -> x = k) |> Array.length
  )
  |> Iter.fold Int.max 0

let () = printf "%d\n%!" ans
