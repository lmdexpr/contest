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
  type t = Int.t
  let zero  = 1_000_000_000
  let one   = 0
  let ( + ) = Int.min
  let ( * ) = Int.add
end

let n = scanf "%d" Fun.id
let h = Array.init n @@ fun _ -> scanf " %d" Fun.id

let h i j = abs @@ h.(j) - h.(i)

let memo = Memo.create n Fun.id
let ans = dp (module MinPlus) ~memo (n - 1) ~solver:(function
  | 0 -> [ 0, None ]
  | 1 -> [ h 0 1, Some 0 ]
  | i -> [
    h i (i - 1), Some (i - 1);
    h i (i - 2), Some (i - 2);
  ]
)

let () = printf "%d\n%!" ans
