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

let n, m = scanf " %d %d" Tuple2.create

let sum = 
  let p = Array.init m ~f:(fun _ -> scanf " %d" Fn.id) in
  Imos.create n @@
  List.init (m - 1) ~f:(fun i -> p.(i), p.(i + 1))

let () =
  Iter.(1 -- (n - 1)) |> Iter.fold (fun acc i ->
    scanf " %d %d %d" @@ fun a b c ->
    acc + min (a * sum.(i)) (b * sum.(i) + c)
  ) 0
  |> printf "%d\n"
