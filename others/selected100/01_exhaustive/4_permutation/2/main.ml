(* https://atcoder.jp/contests/abc150/tasks/abc150_c *)

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
    let downto_loop ~start ~stop ~p ~proc =
      Iter.(start --^ stop) |> Fn.flip Iter.fold_while false
      @@ fun _ i -> if p i then (proc i; true, `Stop) else false, `Continue
    in
    let change_to_next_permutation a ~l ~r =
      downto_loop ~start:(r - 1) ~stop:l
        ~p:(fun i -> a.(l) < a.(i))
        ~proc:(fun i ->
            Array.swap a l i;
            Array.reverse a ~start:(l + 1) ~stop:(r - 1)
          )
    in
    downto_loop ~start:(r - 2) ~stop:l
      ~p:(fun i -> a.(i) < a.(i + 1) && change_to_next_permutation a ~l:i ~r)
      ~proc:ignore

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

let p = Array.init n ~f:(fun _ -> scanf " %d" Int.pred)
let q = Array.init n ~f:(fun _ -> scanf " %d" Int.pred)

let _, a, b =
  Permutation.iter n ~acc:(0, None, None) ~f:(fun (i, a, b) nums ->
      let solve t = Option.some_if (Array.equal (=) nums t) i in
      let solve_if_none x t = if Option.is_none x then solve t else x in
      i + 1, solve_if_none a p, solve_if_none b q
    )

let ans = abs Option.(value_exn a - value_exn b)

let () = printf "%d\n%!" ans
