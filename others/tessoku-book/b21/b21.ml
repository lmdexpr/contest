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
let s = scanf " %s" Fun.id

let memo = Memo.create (succ n * succ n) @@ fun (i, j) -> i * n + j
let ans = dp (module MaxPlus) (1, n) ~memo ~solver:(function
  | l, r when r < l -> [ 0, None ]
  | l, r when l = r -> [ 1, None ]
  | l, r ->
    let rest = [
      0, Some (l+1, r);
      0, Some (l, r-1);
    ] in
    if s.[pred l] <> s.[pred r] then rest
    else
      (2, Some (l+1, r-1)) :: rest
)

let () = printf "%d\n%!" ans
