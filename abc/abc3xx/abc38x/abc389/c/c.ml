open Core
open Scanf

let q = scanf " %d" Fn.id

let a = Array.create ~len:(q + 2) 0
let hd = ref 0
let tl = ref 1

let () =
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 -> 
      let l = scanf " %d" Fn.id in
      a.(!tl) <- l + a.(!tl - 1);
      incr tl
    | 2 -> incr hd
    | 3 -> 
      let k = scanf " %d" Fn.id in
      printf "%d\n%!" @@ a.(!hd + k - 1) - a.(!hd)
    | _ -> 
      assert false
  done
