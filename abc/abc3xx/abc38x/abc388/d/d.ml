open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let sentinel = Array.create ~len:(n + 1) 0

let ans = Array.mapi a ~f:(fun i x ->
  let x = x - (n - (i + 1)) + i + sentinel.(i) in
  sentinel.(i + 1) <- sentinel.(i + 1) + sentinel.(i);
  if x >= 0 then x
  else (
    let j = max 0 n + x in
    sentinel.(j) <- pred sentinel.(j);
    0
  )
)

let () =
  Array.iter ans ~f:(fun x -> printf "%d " x);
  printf "\n%!"
