open Core
open Scanf

module Array = struct
  include Array
  let swap a i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j)  <- tmp

  let rec reverse a ~start ~stop =
    if start < stop then begin
      swap a start stop;
      reverse a ~start:(start + 1) ~stop:(stop - 1)
    end
end

module Permutation = struct
  let next a ~l ~r =
    let rec downto_loop ~start ~stop ~p ~proc =
      if start < stop then false
      else if p start then (proc start; true)
      else
        downto_loop ~start:(start - 1) ~stop ~p ~proc
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
    let nums = Array.init n ~f:Fn.id in
    let rec permutations acc =
      let acc = f acc nums in
      let found_next = next nums ~l:0 ~r:n in
      if found_next then permutations acc else acc
    in
    permutations acc
end

let c = Array.init 9 ~f:(const 0)
let () =
  for i = 0 to 8 do
    scanf "%d " (fun x -> c.(i) <- x)
  done

let disappoint order =
  let disappoint i1 i2 j = c.(i1) = c.(i2) && order.(i1) < order.(j) && order.(i2) < order.(j) in
  [
    0, 1, 2; 3, 4, 5; 6, 7, 8;
    0, 3, 6; 1, 4, 7; 2, 5, 8;
    0, 4, 8; 2, 4, 6
  ]
  |> List.exists ~f:(fun (i, j, k) -> disappoint i j k || disappoint i k j || disappoint j k i)

let not_disappoint, all = Permutation.iter 9
  ~acc:(0, 0)
  ~f:(fun (not_disappoint, all) nums ->
    not_disappoint + Bool.to_int (not @@ disappoint nums), all + 1
  )

let ans = (float not_disappoint) /. (float all)

let () = printf "%.10f\n%!" ans
