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
  type t = float
  let zero = 1e12
  let one  = Float.zero
  let ( + ) = Float.min
  let ( * ) = Float.add
end

let n = scanf " %d" Fun.id

let p = Array.init n (function _ -> scanf " %d %d" @@ fun x y -> x, y)

module BitSet = struct
  let singleton i = 1 lsl i
  let remove s i = s land (lnot @@ singleton i)
  let contains s i = s land singleton i <> 0
end

let (<<) = Fun.compose

let d = Array.make_matrix (n+1) (n+1) MinPlus.zero
let () =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let xi, yi = p.(i) in
      let xj, yj = p.(j) in
      let dx = xi - xj in
      let dy = yi - yj in
      d.(i+1).(j+1) <- Float.sqrt @@ float @@ dx * dx + dy * dy;
    done
  done

let memo = Memo.create (BitSet.singleton n * succ n) @@ fun (i, j) -> i * n + j

let next s i j = d.(i).(j), Some (s, j)

let ans = dp (module MinPlus) ~memo (BitSet.singleton n - 1, 1) ~solver:(function
  | 1, 1                                  -> [ 0., None ]
  | s, 1 when s <> BitSet.singleton n - 1 -> []
  | s, 1 ->
    List.init (n-1) @@ fun j -> next (BitSet.remove s @@ j+1) 1 (j+2)
  | s, i ->
    let s = BitSet.remove s @@ i-1 in
    List.init n Fun.id
    |> List.filter (BitSet.contains s)
    |> List.map (next s i << succ)
)

let () = printf "%.12f\n%!" ans
