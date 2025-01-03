open Core
open Scanf

module SegmentTree (M : sig
  type t
  val idm : t
  val mul : t -> t -> t
end) = struct
  type t = { tree: M.t array; size: int; }

  let create ~len : t = { 
    tree = Array.create ~len:(2 * len) M.idm; 
    size = len; 
  }

  let product { tree; size; _ } l r =
    let rec product lp rp l r =
      if r <= l
      then M.mul lp rp
      else
        product
          (if l mod 2 = 0 then lp else M.mul lp tree.(l))
          (if r mod 2 = 0 then rp else M.mul tree.(r - 1) rp)
          ((l + 1) / 2) (r / 2)
    in
    product M.idm M.idm (l + size) (r + size)

  let update { tree; size; _ } i x =
    let left i = tree.(2 * i) and right i = tree.(2 * i + 1) in
    let rec propagate i =
      let i = i / 2 in
      if 0 < i then 
        (tree.(i) <- M.mul (left i) (right i); propagate i)
    in
    let leaf = i + size in
    tree.(leaf) <- x; propagate leaf

  let of_array a : t =
    let tree = create ~len:Array.(length a) in
    for i = 0 to tree.size - 1 do
      update tree i a.(i)
    done;
    tree

  let max_right { tree; size; _ } l f =
    let rec inner sm l =
      if size <= l then l - size
      else
        let l   = 2 * l in
        let sm' = M.mul sm tree.(l) in
        if f sm' then
          inner sm' (l + 1)
        else
          inner sm l
    in
    let rec loop sm l =
      let l   = l lsr Int.(ctz l) in
      let sm' = M.mul sm tree.(l) in
      if not @@ f sm' then inner sm l
      else
        let l = l + 1 in
        if (l land -l) = l then size 
        else 
          loop sm' l
    in
    if l = size then size
    else 
      loop M.idm (l + size)
end

let n, q = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

module SegTree = SegmentTree (struct
  type t = int
  let idm = -1
  let mul = max
end)
let tree = SegTree.of_array a

let () =
  for _ = 1 to q do
    match scanf " %d %d %d" Tuple3.create with
    | 1, x, v ->                  SegTree.update    tree (x - 1) v
    | 2, l, r -> printf "%d\n" @@ SegTree.product   tree (l - 1) r
    | _, x, v -> printf "%d\n" @@ SegTree.max_right tree (x - 1) (fun x -> x < v) + 1
  done
