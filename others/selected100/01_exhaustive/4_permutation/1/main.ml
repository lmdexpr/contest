(* https://atcoder.jp/contests/abc145/tasks/abc145_c *)

open Core
open Scanf

module Array = struct
  include Array
  let rec reverse a ~start ~stop =
    if start < stop then begin
      Array.swap a start stop;
      reverse a ~start:(start + 1) ~stop:(stop - 1)
    end
end

module Permutation = struct
  let next a ~l ~r =
    let stop = true, `Stop and continue = false, `Continue in
    let change_to_next_permutation a ~l ~r =
      Iter.((r - 1) --^ l) |> Fn.flip Iter.fold_while false
      @@ fun _ j ->
      if a.(l) >= a.(j) then continue
      else begin
        Array.swap a l j;
        Array.reverse a ~start:(l + 1) ~stop:(r - 1);
        stop
      end
    in
    Iter.((r - 2) --^ l) |> Fn.flip Iter.fold_while false
    @@ fun _ i ->
    let success = 
      a.(i) < a.(i + 1) &&
      change_to_next_permutation a ~l:i ~r
    in
    if success then stop else continue

  let iter n ~f ~acc =
    let nums = Array.init n ~f:ident in
    let rec permutations acc =
      let acc = f acc nums in
      let found_next = next nums ~l:0 ~r:n in
      if found_next then permutations acc else acc
    in
    permutations acc
end

let n = scanf "%d" ident
let pos = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let dist (x1, y1) (x2, y2) =
  let dx = x1 - x2 and dy = y1 - y2 in
  Float.sqrt (Float.of_int @@ dx * dx + dy * dy)

let ans = Permutation.iter n ~acc:0. ~f:(fun acc permutation ->
    let pos x = pos.(permutation.(x)) in
    Iter.(0 -- (n - 2))
    |> Iter.map (fun i -> dist (pos i) (pos @@ i + 1)) |> Iter.sumf
    |> (+.) acc
  )

let fact = Iter.(1 -- n) |> Iter.fold ( * ) 1
let ans = ans /. (Float.of_int fact)

let () = printf "%.10f\n%!" ans
