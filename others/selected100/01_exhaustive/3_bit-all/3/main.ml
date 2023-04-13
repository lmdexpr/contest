(* https://atcoder.jp/contests/abc002/tasks/abc002_4 *)

open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let friend = Array.init n ~f:(fun i -> Array.init n ~f:(fun j -> i = j))
let () =
  for _ = 1 to m do
    let x, y = scanf " %d %d" @@ fun x y -> x-1, y-1 in
    friend.(x).(y) <- true;
    friend.(y).(x) <- true
  done

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0

  let bits len = 0 -- (1 lsl len - 1)
  let by_bits x = map (fun bits -> (1 -- x) |> filter (on bits) |> map (fun x -> x - 1) |> to_array)
  let start x = by_bits x (bits x)
end

let ans =
  Bit_all.start n
  |> Iter.filter (fun members ->
      Array.for_all members ~f:(fun i ->
          Array.for_all members ~f:(fun j -> friend.(i).(j))
        )
    )
  |> Iter.map (Array.length)
  |> Iter.max ~lt:(<)
  |> Option.value ~default:0

let () = printf "%d\n%!" ans
