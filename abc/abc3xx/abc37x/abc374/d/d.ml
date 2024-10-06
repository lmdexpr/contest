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

module P = Permutation (Int)

module Bit_all = struct
  open Iter
  let on x i = x land (1 lsl (i - 1)) <> 0
 
  let bits len = 0 -- (1 lsl len - 1)
  let by_bits x = map (fun bits -> (1 -- x) |> filter (on bits) |> map (fun x -> x - 1) |> to_array)
  let start x = by_bits x (bits x)
end

let n     = scanf "%d" Fn.id
let s, t  = scanf " %f %f" Tuple2.create
let lines = Array.init n ~f:(fun _ -> 
  scanf " %d %d %d %d" @@ fun a b c d -> (a, b), (c, d)
)

let dist (a, b) (c, d) = 
  let x = c - a in let x = x * x in let x = Float.of_int x in
  let y = d - b in let y = y * y in let y = Float.of_int y in
  Float.(sqrt (x + y))

let calc idxes =
  let lines = Array.map idxes ~f:(fun i -> lines.(i)) in
  Bit_all.start n
  |> Iter.map (fun bits ->
    let lines = Array.copy lines in
    Array.iter bits ~f:(fun i -> lines.(i) <- Tuple2.swap lines.(i));
    Array.to_list lines
  )
  |> Iter.map (fun lines ->
    let rec loop acc (x, y) = function
      | []                       -> acc
      | ((a, b), (c, d)) :: rest ->
        let time = dist (x, y) (a, b) /. s +. dist (a, b) (c, d) /.t in
        loop (acc +. time) (c, d) rest
    in
    loop 0. (0, 0) lines
  )
  |> Iter.fold Float.min Float.infinity

let ans = P.fold Array.(init n ~f:Fn.id) n 
  ~acc:Float.infinity
  ~f:Float.(fun acc idxes -> min acc (calc idxes))

let () = printf "%.10f\n%!" ans
