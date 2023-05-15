open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let partition a ~start ~left ~right =
  let stop = Array.length a in
  let rec aux i lacc racc =
    if i >= stop then List.to_array lacc, List.to_array racc
    else
      aux (i + 1)
        (if left  a.(i) then i :: lacc else lacc)
        (if right a.(i) then i :: racc else racc)
  in
  aux start [] []

let rec solve i l r =
  if i >= n then 0, 0
  else
    let pivot = a.(i) in
    let left, right =
      partition a ~start:(i+1) ~left:(fun a -> a < pivot) ~right:(fun a -> a > pivot)
    in

    let len_left = Array.length left and len_right = Array.length right in

    let asc i j = Int.ascending  a.(i) a.(j) and dsc i j = Int.descending a.(i) a.(j) in

    if      k - l < len_left  then (Array.sort ~compare:asc left;  i,  left.(k - l))
    else if r - k < len_right then (Array.sort ~compare:dsc right; i, right.(r - k))
    else
      solve (i + 1) (l + len_left) (r - len_right)

let l, r = solve 0 1 (n * (n + 1) / 2)

let () =
  let p i = printf "%d " a.(i) in
  for i = 0 to l - 1 do p i done;
  for i = r downto l do p i done;
  for i = r + 1 to n - 1 do p i done;
  printf "\n%!"
