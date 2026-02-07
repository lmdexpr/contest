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

let restore (type t) (module R : Rig with type t = t) ~memo ~solver k  =
  let ans = dp (module R) ~memo ~solver k in
  let rec go acc k =
    let acc = k :: acc in
    solver k
    |> List.find_map (function
      | (_, None)   -> None
      | (w, Some j) ->
        match Memo.( memo.@(k), memo.@(j) ) with
        | Some v, Some u when v = R.(w * u) -> Some j
        | _ -> None
    )
    |> Option.fold ~none:acc ~some:(go acc)
  in
  ans, go [] k

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
let _, path = restore (module Boolean) ~memo (n, s) ~solver:(function
  | (_, 0) -> [ true,  None ]
  | (0, _) -> [ false, None ]
  | (i, j) -> 
    (true, Some (i-1, j)) ::
    if j < a.(i-1) then []
    else [
      true, Some (i-1, j - a.(i-1));
    ]
)

let () =
  if snd @@ List.hd path <> 0 then (
    printf "-1\n%!";
    exit 0
  )

let ans =
  let [@tail_mod_cons] rec go = function
    | (_, p) :: ((_, q) :: _ as r) when p = q -> go r
    | (_, _) :: ((i, _) :: _ as r)            -> i :: go r
    | _ -> []
  in
  go path

let () =
  printf "%d\n%!" @@ List.length ans;
  ans |> List.iter (printf "%d "); printf "\n%!";
