open Core
open Scanf

module ImosMake (M : sig include Int_intf.S end) = struct
  type t = M.t array

  let init n : t = Array.create ~len:(n + 2) M.zero

  let update t (x, y) =
    let l, r = min x y, max x y in
    t.(l) <- M.succ t.(l);
    t.(r) <- M.pred t.(r)

  let finish imos : t = 
    let cumsum ~init ~f a =
      let paired f a b = let r = f a b in r, r in
      Array.folding_map a ~init ~f:(paired f)
    in
    cumsum ~init:M.zero ~f:M.(+) imos

  let create n xs : t =
    let t = init n in
    List.iter xs ~f:(update t);
    finish t
end
module Imos = ImosMake(Int)

let n = scanf "%d" Fn.id

let len = 1_000_000

let imos =
  Imos.create len @@
  List.init n ~f:(fun _ ->
    scanf " %d %d" @@ fun a b -> a, b + 1
  )

let ans = Array.fold imos ~init:0 ~f:Int.max

let () = printf "%d\n%!" ans
