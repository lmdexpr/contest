open Core
open Scanf

let q = scanf " %d" Fn.id

let a = Array.create ~len:200_005 (0, 0)

let head = ref 0
let last = ref 0

let take k a =
  let rec aux acc k =
    if k = 0 then acc
    else
      let c, x = a.(!head) in
      if c <= k then (
        incr head;
        aux (acc + c * x) (k - c)
      ) else (
        a.(!head) <- (c - k, x);
        aux (acc + k * x) 0
      )
  in
  aux 0 k

let () =
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 ->
      let c = scanf " %d" Fn.id in
      let x = scanf " %d" Fn.id in
      a.(!last) <- (c, x);
      incr last;
    | _ ->
      let k = scanf " %d" Fn.id in
      printf "%d\n%!" (take k a);
  done
