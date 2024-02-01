open Core
open Scanf

module SegmentTree = struct
  type m                = int
  let idm : m           = 0
  let mul : m -> m -> m = max

  type t = { tree: m array; size: int }

  let create ~len : t = { tree = Array.create ~len:(2 * len) idm; size = len }

  let product { tree; size } l r =
    let rec product lp rp l r =
      if r <= l
      then mul lp rp
      else
        product
          (if l mod 2 = 0 then lp else mul lp tree.(l))
          (if r mod 2 = 0 then rp else mul tree.(r - 1) rp)
          ((l + 1) / 2) (r / 2) 
    in
    product idm idm (l + size) (r + size)

  let update { tree; size } i x =
    tree.(i + size) <- x;
    let left i = 2 * i and right i = 2 * i + 1 in
    let rec propagate i =
      if 0 < i then begin
        tree.(i) <- mul tree.(left i) tree.(right i);
        propagate (i / 2)
      end
    in
    propagate ((i + size) / 2)

  module Infix = struct
    let (.*()) t (l, r) = product t l r
    let (.!()<-) t i x = update t i x
  end
end

open SegmentTree.Infix

let n, d = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let upper = 500010
let tree  = SegmentTree.create ~len:(upper + 1)
let () =
  Array.iter a ~f:(fun x ->
    let l = max (x - d) 0
    and r = min (x + d) upper + 1 in
    tree.!(x) <- tree.*(l, r) + 1
  )

let ans = tree.*(0, upper)

let () = printf "%d\n%!" ans
