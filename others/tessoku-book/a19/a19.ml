open Scanf
open Printf

module Memo = struct
  type ('a, 'b) t = { memo : 'a option array; index_of : 'b -> int }

  let create size index_of = { memo = Array.make size None; index_of }

  let (.@()) t k = Array.get t.memo (t.index_of k)

  let recursive t x f =
    let rec g x =
      match t.@(x) with
      | Some v -> v
      | None   -> let v = f g x in t.memo.(t.index_of x) <- Some v; v
    in
    g x
end

module type Rig = sig
  type t
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
end

let dp (type t) (module R : Rig with type t = t) ~memo ~solver k =
  Memo.recursive memo k @@ fun self k ->
  List.fold_left
    R.(fun acc (u, j) -> acc + u * Option.(map self j |> value ~default:R.one))
    R.zero
    (solver k)

module MaxPlus = struct
  type t = int
  let zero = Int.min_int
  let one  = Int.zero
  let ( + ) = Int.max
  let ( * ) = Int.add
end

let n = scanf " %d" Fun.id
let w = scanf " %d" Fun.id

let goods = Array.init n (fun _ -> scanf " %d %d" @@ fun w v -> w, v)

let memo = Memo.create (succ n * succ w) (fun (i, j) -> i * (w + 1) + j)
let ans = dp (module MaxPlus) ~memo (n, w) ~solver:(function
  | (0, _) -> [ 0, None ]
  | (i, c) ->
    let wi, vi = goods.(i-1) in
    (0, Some (i-1, c)) ::
    if c < wi then []
    else
      [ vi, Some (i-1, c - wi); ]
)

let () = printf "%d\n%!" ans
