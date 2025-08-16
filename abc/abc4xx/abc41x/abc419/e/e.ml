open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id
let l = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id % m)

let inf = 1_000_000_001
let ch f a i v = a.(i) <- f a.(i) v

let ans =
  Iter.(0 -- pred l) |> Fn.flip Iter.fold
    (
      let dp = Array.create ~len:m inf in dp.(0) <- 0;
      dp
    )
    (fun dp i ->
      let pd = Array.create ~len:m inf in
      for j = 0 to m - 1 do
        let cost =
          Iter.(int_range_by ~step:l i (pred n))
          |> Fn.flip Iter.fold 0 (fun acc k ->
            acc + j - a.(k) + if j >= a.(k) then 0 else m
          )
        in
        for k = 0 to m - 1 do
          ch min pd ((k + j) % m) (dp.(k) + cost)
        done
      done;
      pd
    )
  |> Fn.flip Array.get 0

let () = printf "%d\n%!" ans
