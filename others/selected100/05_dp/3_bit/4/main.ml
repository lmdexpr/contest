(* https://atcoder.jp/contests/joi2017yo/tasks/joi2017yo_d *)
open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let cnt = Array.create ~len:m 0
let cum = Array.make_matrix ~dimx:m ~dimy:(n + 1) 0
let () =
  for i = 1 to n do
    let doll = scanf " %d" Int.pred in
    cnt.(doll)     <- cnt.(doll) + 1;
    cum.(doll).(i) <- 1;
  done;
  Array.iter cum ~f:(fun cum ->
      for i = 1 to n do
        cum.(i) <- cum.(i) + cum.(i - 1)
      done)

module Bits = struct
  let on  bits v = bits land (1 lsl v) <> 0
  let non bits v = bits land (1 lsl v) =  0
end

let inf = 1 lsl 30
let dp  = Array.create ~len:(1 lsl m) inf

let () =
  dp.(0) <- 0;
  for bits = 0 to 1 lsl m - 1 do
    let offset =
      Iter.(0 -- (m - 1))
      |> Iter.filter (Bits.on bits)
      |> Iter.map    (Array.get cnt)
      |> Iter.sum
    in
    Iter.(0 -- (m - 1))
    |> Iter.filter (Bits.non bits)
    |> Iter.map (fun i ->
        1 lsl i, cnt.(i), cum.(i).(cnt.(i) + offset) - cum.(i).(offset)
      )
    |> Iter.iter (fun (ibit, cnt, sum) ->
        dp.(bits + ibit) <- min dp.(bits + ibit) (dp.(bits) + cnt - sum)
      )
  done

let ans = dp.(1 lsl m - 1)
let ()  = printf "%d\n%!" ans
