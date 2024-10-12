open Core
open Scanf

let s = scanf "%s" Fn.id

let chr_to_idx = Array.create ~len:26 []
let () =
  String.iteri s ~f:(fun i c ->
    let c = Char.to_int c - Char.to_int 'A' in
    chr_to_idx.(c) <- i :: chr_to_idx.(c)
  )

let solve acc lst = 
  let a = List.to_array lst in
  let n = Array.length a in
  let cumsum =
    Array.folding_map a ~init:0 ~f:(fun acc x -> acc + x, acc + x)
  in
  Array.foldi a ~init:acc ~f:(fun i acc x ->
    acc + (x - 1) * (n - 1 - i) - (cumsum.(n - 1) - cumsum.(i))
  )

let solve acc = function
  | [] | [ _ ] -> acc
  | lst        -> solve acc lst

let ans = Array.fold chr_to_idx ~init:0 ~f:solve

let () = printf "%d\n%!" ans
