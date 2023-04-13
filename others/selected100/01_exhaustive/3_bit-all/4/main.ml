(* https://atcoder.jp/contests/joi2008yo/tasks/joi2008yo_e *)

open Core
open Scanf

let r, c = scanf "%d %d" Tuple2.create
let a = Array.init c ~f:(const 0)
let () =
  for j = 0 to r - 1 do
    for i = 0 to c - 1 do
      if scanf " %d" ((=) 0) then
        a.(i) <- a.(i) lor (1 lsl j)
    done
  done

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0

  let bits len = 0 -- (1 lsl len - 1)
  let by_bits x = map (fun bits -> (1 -- x) |> filter (on bits) |> map (fun x -> x - 1))
  let start x = by_bits x (bits x)
end

let ans =
  Bit_all.start r
  |> Iter.map (fun choiced ->
      let a = Array.copy a in
      choiced |> Iter.iter (fun i ->
          for j = 0 to c - 1 do
            a.(j) <- a.(j) lxor (1 lsl i)
          done
        );
      Iter.(0 -- (c - 1))
      |> Iter.map (fun i ->
          let popcount = Int.popcount a.(i) in
          max (r - popcount) popcount
        )
      |> Iter.sum
    )
  |> Iter.max_exn ~lt:(<)

let () = printf "%d\n%!" ans
