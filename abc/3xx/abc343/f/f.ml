open Core
open Scanf

module SegmentTree = struct
  type m      = (int * int) * (int * int)
  let idm : m = (0, 0), (0, 0)

  let mul ((max1, mc1), s1) ((max2, mc2), s2) =
    let max_and_mc (x1, c1) (x2, c2) =
      if x1 = x2 then x1, c1 + c2
      else if x1 > x2 then x1, c1
      else x2, c2
    in
    let sec = max_and_mc s1 s2 in
    if max1 = max2 then
      (max1, mc1 + mc2), sec
    else if max1 > max2 then
      (max1, mc1), max_and_mc (max2, mc2) sec
    else
      (max2, mc2), max_and_mc (max1, mc1) sec

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
    tree.(i + size) <- (x, 1), (0, 0);
    let left i = 2 * i and right i = 2 * i + 1 in
    let rec propagate i =
      if 0 < i then begin
        tree.(i) <- mul tree.(left i) tree.(right i);
        propagate (i / 2)
      end
    in
    propagate ((i + size) / 2)
end

let n, q = scanf "%d %d" Tuple2.create

let st = SegmentTree.create ~len:n
let () =
  for i = 0 to n - 1 do
    scanf " %d" @@ SegmentTree.update st i 
  done

let () =
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 ->
      let p, x = scanf " %d %d" Tuple2.create in
      SegmentTree.update st (p - 1) x 
    | _ ->
      let l, r = scanf " %d %d" Tuple2.create in
      let _, (_, ans) = SegmentTree.product st (l-1) r in
      printf "%d\n" ans
  done
