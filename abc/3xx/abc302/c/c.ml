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

let n, m = scanf "%d %d" Tuple2.create
let s = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)

let count = Array.make_matrix ~dimx:n ~dimy:n 0
let () =
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      for k = 0 to m - 1 do
        if Char.(s.(i).(k) <> s.(j).(k)) then begin
          count.(i).(j) <- count.(i).(j) + 1;
          count.(j).(i) <- count.(j).(i) + 1
        end
      done
    done
  done

let () =
  Permutation.iter n ~acc:() ~f:(fun _ nums ->
      if
        Iter.(1 -- (n - 1))
        |> Iter.for_all (fun i -> count.(nums.(i - 1)).(nums.(i)) = 1)
      then 
        ( printf "Yes\n%!"; exit 0 )
    );
  printf "No\n%!"
