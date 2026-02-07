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
  type t = Int64.t
  let zero = 1_000_000_000_000_000L
  let one  = Int64.zero
  let ( + ) = Int64.min
  let ( * ) = Int64.add
end

let n = scanf " %d" Fun.id
let w = scanf " %Ld" Fun.id

let goods = Array.init n (fun _ -> scanf " %Ld %d" @@ fun w v -> w, v)

let v = Array.fold_left (fun acc (_, v) -> acc + v) 0 goods

let memo = Memo.create (succ n * succ v) (fun (i, j) -> i * (v + 1) + j)
let dp = dp (module MinPlus) ~memo ~solver:(function
  | (0, 0) -> [ MinPlus.one , None ]
  | (0, _) -> [ MinPlus.zero, None ]
  | (i, v) ->
    let wi, vi = goods.(i - 1) in
    (MinPlus.one, Some (i - 1, v)) ::
    if v < vi then []
    else 
      [ wi, Some (i - 1, v - vi) ]
)

let ans =
  Seq.init v (fun x -> v - x)
  |> Seq.find (fun v -> dp (n, v) <= w)
  |> Option.value ~default:0

let () = printf "%d\n%!" ans
