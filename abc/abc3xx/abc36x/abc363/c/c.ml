open Core
open Scanf

let n, k = scanf "%d %d\n" Tuple2.create
let s    = scanf "%s" String.to_array

module Array = struct
  include Array
  let rec reverse a ~start ~stop =
    if start < stop then begin
      Array.swap a start stop;
      reverse a ~start:(start + 1) ~stop:(stop - 1)
    end
end

module Permutation (M: sig type t val compare: t -> t -> int end) = struct
  let next a ~l ~r =
    let downto_loop ~start ~stop ~p ~proc =
      Iter.(start --^ stop) |> Fn.flip Iter.fold_while false
      @@ fun _ i -> if p i then (proc i; true, `Stop) else false, `Continue
    in
    let change_to_next_permutation a ~l ~r =
      downto_loop ~start:(r - 1) ~stop:l
        ~p:(fun i -> M.compare a.(l) a.(i) < 0)
        ~proc:(fun i ->
            Array.swap a l i;
            Array.reverse a ~start:(l + 1) ~stop:(r - 1)
          )
    in
    downto_loop ~start:(r - 2) ~stop:l
      ~p:(fun i -> 
        M.compare a.(i) a.(i + 1) < 0 && 
        change_to_next_permutation a ~l:i ~r
      )
      ~proc:ignore

  let fold arr n ~f ~acc =
    let arr = Array.copy arr in
    Array.sort arr ~compare:M.compare;
    let rec permutations acc =
      let acc = f acc arr in
      let found_next = next arr ~l:0 ~r:n in
      if found_next then permutations acc else acc
    in
  permutations acc
end

module PermutationChar = Permutation (Char)

let contains_palindrome s =
  let rec is_palindrome s ~l ~r =
    if l >= r then true
    else 
      Char.(s.(l) = s.(r)) && is_palindrome s ~l:(l + 1) ~r:(r - 1)
  in
  let rec loop i =
    if Array.length s < i + k then false
    else if is_palindrome s ~l:i ~r:(i + k - 1) then true
    else loop (i + 1)
  in
  loop 0

let ans = 
  PermutationChar.fold s n ~acc:0 ~f:(fun acc s ->
    acc + Bool.to_int (not @@ contains_palindrome s)
  )

let () = printf "%d\n%!" ans
