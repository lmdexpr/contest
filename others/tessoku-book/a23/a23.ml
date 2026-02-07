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

let n = scanf " %d" Fun.id
let m = scanf " %d" Fun.id

let bit i = 1 lsl i

let a = Array.make (m+1) 0
let () =
  for i = 1 to m do
    for j = 0 to n-1 do
      if scanf " %d" ((=) 1) then a.(i) <- a.(i) lor bit j
    done
  done

let memo = Memo.create (succ m * (bit @@ n + 1)) @@ fun (i, s) -> i * bit n + s

let ans = dp (module MinPlus) ~memo (1, 0) ~solver:(function
  | i, s when i > m && s = 1 lsl n - 1 -> [ 0, None ]
  | i, _ when i > m           -> []
  | i, s when s lor a.(i) = s -> [ 0, Some (i+1, s) ]
  | i, s -> [
    1, Some (i+1, s lor a.(i));
    0, Some (i+1, s);
  ]
)

let ans = if ans = MinPlus.zero then -1 else ans

let () = printf "%d\n%!" ans
