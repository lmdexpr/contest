open Core
open Scanf

let t = scanf " %d" Fn.id
let n = scanf " %d" Fn.id

let a = Array.create ~len:(t+2) 0

let () =
  for _ = 1 to n do
    let (.+()<-) a i v = a.(i) <- a.(i) + v in
    a.+(scanf " %d" Fn.id) <- 1;
    a.+(scanf " %d" Fn.id) <- -1;
  done

let cumsum ~init ~f a =
  let paired f a b = let r = f a b in r, r in
  Array.folding_map a ~init ~f:(paired f)

let cumsum = cumsum ~init:0 ~f:(+) a

let () =
  for i = 0 to t-1 do
    printf "%d\n" cumsum.(i)
  done
