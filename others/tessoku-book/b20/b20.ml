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

module MinPlus = struct
  type t = int
  let zero = 1_000_000_000
  let one  = Int.zero
  let ( + ) = Int.min
  let ( * ) = Int.add
end

let s = scanf " %s" Fun.id
let t = scanf " %s" Fun.id

let n = String.length s
let m = String.length t

let memo = Memo.create ((n + 2) * (m + 2)) (fun (i, j) -> i * (m + 1) + j)
let ans = dp (module MinPlus) ~memo (1, 1) ~solver:(function
  | (i, j) when n < i && m < j    -> [ 0, None ]
  | (i, j) when n < i             -> [ 1, Some (i, j+1) ]
  | (i, j) when m < j             -> [ 1, Some (i+1, j) ]
  | (i, j) when s.[i-1] = t.[j-1] -> [ 0, Some (i+1, j+1); ]
  | (i, j) ->
    [
      1, Some (i+1, j);
      1, Some (i, j+1);
      1, Some (i+1, j+1);
    ]
)

let () = printf "%d\n%!" ans
