open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let b = Array.sorted_copy a ~compare
let cumsum = Array.folding_map b ~init:0 ~f:(fun acc x -> (acc + x, acc + x))

let () =
  Array.iter a ~f:(fun x ->
    let j = Array.binary_search b ~compare `First_strictly_greater_than x in
    let j = Option.value j ~default:n in
    let ans = cumsum.(n - 1) - cumsum.(j - 1) in
    printf "%d " ans
  );
  printf "\n"
