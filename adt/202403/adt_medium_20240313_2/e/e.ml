open Core
open Scanf

let s, k = scanf "%s %d" Tuple2.create

let s  = String.to_array s
let () = Array.sort s ~compare:Char.compare

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

  let fold n ~f ~acc =
    let nums = Array.init n ~f:Fn.id in
    let rec permutations acc =
      let acc = f acc nums in
      let found_next = next nums ~l:0 ~r:n in
      if found_next then permutations acc else acc
    in
  permutations acc
end

let n = Array.length s

let ss =
  Permutation.fold n ~acc:String.Set.empty ~f:(fun set a ->
    let buf = Buffer.create n in
    Array.iter a ~f:(fun i -> Buffer.add_char buf s.(i));
    Set.add set (Buffer.contents buf)
  )

let () =
  Set.nth ss (k - 1)
  |> Option.value_exn
  |> printf "%s\n"
