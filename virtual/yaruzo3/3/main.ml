open Core
open Scanf

let rec reverse_in_range a ~start ~stop =
  if start < stop then begin
    Array.swap a start stop;
    reverse_in_range a ~start:(start + 1) ~stop:(stop - 1)
  end

let stop = true, `Stop and continue = false, `Continue

let change_to_next_permutation a ~l ~r =
  Iter.((r - 1) --^ l) |> Iter.fold_while
    (fun _ j ->
       if a.(l) < a.(j) then begin
         Array.swap a l j;
         reverse_in_range a ~start:(l + 1) ~stop:(r - 1);
         stop
       end
       else continue
    )
    false

let next_permutation a ~l ~r =
  Iter.((r - 2) --^ l) |> Iter.fold_while
    (fun _ i ->
       let success = a.(i) < a.(i + 1) && change_to_next_permutation a ~l:i ~r in
       if success then stop else continue
    )
    false

let permutations n ~f ~acc =
  let nums = Array.init n ~f:ident in
  let rec permutations acc =
    let acc = f acc nums in
    let found_next = next_permutation nums ~l:0 ~r:n in
    if found_next then permutations acc else acc
  in
  permutations acc

let a = scanf "%d" ident
let b = scanf " %d" ident
let c = scanf " %d" ident
let d = scanf " %d" ident
let e = scanf " %d" ident

let order = [| a; b; c; d; e |]

let ans = permutations 5 ~acc:1000 ~f:(fun ans idx ->
    min ans @@ Array.fold idx ~init:0 ~f:(fun t i -> order.(i) + t / 10 * 10 + Bool.to_int (t mod 10 <> 0) * 10)
  )

let () = printf "%d\n%!" ans
