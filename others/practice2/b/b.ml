open Core
open Scanf

module Fenwick = struct
  module M = Int

  type t = { n : int; a : M.t array }

  let create n = { n; a = Array.create ~len:n M.zero }

  let add t i x =
    let rec go i = if i < t.n then (
      t.a.(i) <- M.(t.a.(i) + x);
      go (i lor (i + 1))
    ) in
    go i

  let sum t l r =
    let rec go i acc = 
      if i < 0 then acc
      else 
        go (i land (i + 1) - 1) M.(acc + t.a.(i))
    in
    M.(go Int.(pred r) zero - go Int.(pred l) zero)
end

let n, q = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let tree = Fenwick.create n
let () =
  for i = 0 to n - 1 do
    Fenwick.add tree i a.(i)
  done;
  for _ = 1 to q do
    scanf " %d %d %d" @@ function
    | 0 -> Fenwick.add tree
    | 1 -> fun l r -> printf "%d\n" @@ Fenwick.sum tree l r
    | _ -> assert false
  done

