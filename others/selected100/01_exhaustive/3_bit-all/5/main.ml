(* https://atcoder.jp/contests/s8pc-4/tasks/s8pc_4_b *)

open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %Ld" ident)

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0

  let bits len = 0 -- (1 lsl len - 1)
  let by_bits k x = filter_map (fun bits ->
      Option.some_if
        (Int.popcount bits = k)
        ((1 -- x) |> filter (on bits) |> map (fun x -> x - 1))
    )
  let start k x = by_bits k x (bits x)
end

let hurdle_interval start stop =
  Iter.(start -- stop)
  |> Iter.map (Array.get a)
  |> Iter.max ~lt:Int64.(<)
  |> Option.value ~default:0L

let ans =
  Bit_all.start k n
  |> Iter.map
    (Fn.flip Iter.fold (0L, 0L, 0)
       (fun (money, hurdle, pred) i ->
          let hurdle = Int64.max hurdle @@ hurdle_interval pred (i - 1) in
          Int64.(money + max 0L (hurdle - a.(i)), max hurdle a.(i) + 1L, i)
       )
    )
  |> Iter.map Tuple3.get1
  |> Iter.min_exn ~lt:Int64.(<)

let () = printf "%Ld\n%!" ans
