open Core

module SegmentTree (M : sig
  type t
  val idm : t
  val mul : t -> t -> t
end) = struct
  type t = { tree: M.t array; size: int }

  let create ~len : t = { tree = Array.create ~len:(2 * len) M.idm; size = len }

  let product { tree; size } l r =
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

  let update { tree; size } i x =
    tree.(i + size) <- x;
    let left i = 2 * i and right i = 2 * i + 1 in
    let rec propagate i =
      if 0 < i then begin
        tree.(i) <- M.mul tree.(left i) tree.(right i);
        propagate (i / 2)
      end
    in
    propagate ((i + size) / 2)
end

module Fenwick (M : sig 
  type t 
  val zero : t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
end) = struct

  type t = { n : int; a : M.t array }

  let create n = { n; a = Array.create ~len:n M.zero }

  let add t i x =
    let rec go i = if i < t.n then (
      t.a.(i) <- M.(t.a.(i) + x);
      go (i lor (i + 1))
    ) in
    go i

  let sum t l r =
    let rec go i acc = 
      if i < 0 then acc
      else 
        go (i land (i + 1) - 1) M.(acc + t.a.(i))
    in
    M.(go Int.(pred r) zero - go Int.(pred l) zero)
end

(* no test *)
module LazySegmentTree = struct
  module M = Int

  type m                = M.t
  let idm : m           = M.zero
  let mul : m -> m -> m = M.max

  type t = { tree: m array; lazy_: m array; size: int; }

  let create ~len : t = { 
    tree  = Array.create ~len:(2 * len) idm; 
    lazy_ = Array.create ~len:(2 * len) idm;
    size  = len;
  }


  let left i = 2 * i 
  let right i = 2 * i + 1

  let eval { tree; lazy_; size } k =
    if M.(lazy_.(k) = idm) then ()
    else (
      if k < size - 1 then (
        lazy_.(left k)  <- lazy_.(k);
        lazy_.(right k) <- lazy_.(k)
      );
      tree.(k)  <- lazy_.(k);
      lazy_.(k) <- idm
    )

  (* [l, r) *)
  let product ({ tree; lazy_ = _; size } as t) l r =
    let rec product k a b =
      eval t k;
      if r <= a || b <= l then idm
      else if l <= a && b <= r then tree.(k)
      else
        let m  = (a + b) / 2 in
        let vl = product (left k)  a m in
        let vr = product (right k) m b in
        mul vl vr
    in
    product 0 0 size

  (* [l, r) *)
  let update ({ tree; lazy_; size } as t) l r x =
    let rec propagate k a b =
      eval t k;
      if l <= a && b <= r then (lazy_.(k) <- x; eval t k) 
      else if r < b && a < l then (
        let m = (a + b) / 2 in
        propagate (left k)  a m;
        propagate (right k) m b;
        tree.(k) <- mul tree.(left k) tree.(right k)
      )
    in
    propagate 0 0 size
end
