open Core
open Scanf

let modulo = 998244353
let (+%) a b = (a + b) % modulo
and (-%) a b = (a - b + modulo) % modulo
and( *%) a b = a * b % modulo

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

let n = scanf "%d" Fn.id

let _a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let ans = 0

let () = printf "%d\n%!" ans
