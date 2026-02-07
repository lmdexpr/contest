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

module Boolean = struct
  type t = Bool.t
  let zero  = false
  let one   = true
  let ( + ) = (||)
  let ( * ) = (&&)
end

let n = scanf " %d" Fun.id
let s = scanf " %d" Fun.id
let a = Array.init n (fun _ -> scanf " %d" Fun.id)

let memo = Memo.create (succ n * succ s) (fun (i, j) -> i * (s + 1) + j)
let yes = dp (module Boolean) ~memo (n, s) ~solver:(function
    | (_, 0) -> [ true,  None ]
    | (0, _) -> [ false, None ]
    | (i, j) -> 
      (true, Some (i-1, j)) ::
      if j < a.(i-1) then []
      else [
        true, Some (i-1, j - a.(i-1));
      ]
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
