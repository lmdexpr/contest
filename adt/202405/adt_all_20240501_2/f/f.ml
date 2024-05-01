open Core
open Scanf

let x = scanf "%s" String.to_array

let n = scanf " %d" Fn.id
let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id)

let compare s t =
  let find_index c = fst @@ Array.findi_exn x ~f:Char.(fun _ x -> x = c) in
  let (<) c d = 
    find_index c < find_index d
  in
  let (>) c d = not (c < d) && Char.(c <> d) in
  let len_s = String.length s in
  let len_t = String.length t in
  let rec loop i =
    if i = len_s && i = len_t then 0
    else if i = len_s then -1
    else if i = len_t then 1
    else if s.[i] < t.[i] then -1
    else if s.[i] > t.[i] then 1
    else loop (i + 1)
  in
  loop 0

let () = Array.sort s ~compare

let () = Array.iter s ~f:(printf "%s\n");
