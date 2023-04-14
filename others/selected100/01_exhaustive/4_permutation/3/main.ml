(* https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=ALDS1_13_A&lang=ja *)

open Scanf

let id x = x

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

  let equal f a b =
    let len = Array.length a in
    let rec loop i = i = len || f a.(i) b.(i) && loop (i + 1) in
    len = Array.length b && loop 0
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
    let nums = Array.init n id in
    let rec permutations acc =
      let acc = f acc nums in
      let found_next = next nums ~l:0 ~r:n in
      if found_next then permutations acc else acc
    in
    permutations acc
end

let k = scanf "%d" id
let board = Array.init 8 (fun _ -> -1)
let () =
  for _ = 1 to k do
    scanf " %d %d" @@ Array.set board
  done

module SI = Set.Make(Int32)

let () = Permutation.iter 8 ~acc:() ~f:(fun () permutation ->
    let no_duplication f = Array.map f permutation |> Array.to_list |> SI.of_list |> SI.cardinal = 8 in
    if
      Array.equal (fun a b -> a = -1 || a = b) board permutation &&
      no_duplication Int32.(fun i -> of_int @@ permutation.(i) + i) &&
      no_duplication Int32.(fun i -> of_int @@ permutation.(i) - i)
    then (
      for i = 0 to 7 do
        for j = 0 to 7 do
          print_char @@ if permutation.(i) = j then 'Q' else '.'
        done;
        print_newline ()
      done;
      exit 0
    )
  )
