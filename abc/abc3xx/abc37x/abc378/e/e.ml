open Core
open Scanf

module Fenwick = struct
  module M = Int

  type t = { n : int; a : M.t array }

  let create n = { n; a = Array.create ~len:Int.(succ n) M.zero }

  open struct 
    let lowbit x = x land (-x) 
  end

  let add t i x =
    let rec go i = if i <= t.n then (
      t.a.(i) <- M.(t.a.(i) + x);
      go (i + lowbit i)
    ) in
    go i

  let sum t l r =
    let rec go i acc = 
      if i <= 0 then acc
      else 
        go (i - lowbit i) M.(acc + t.a.(i))
    in
    M.(go Int.(pred r) zero - go Int.(pred l) zero)
end

let n, m = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let cumsum a =
  let paired f a b = let r = f a b in r, r in
  Array.folding_map a ~init:0 ~f:(paired (fun a b -> (a + b) % m))

let s = Array.append [| 0 |] @@ cumsum a

let tree = Fenwick.create m
let ans, _ =
  Iter.(1 -- n)
  |> Iter.fold 
    (fun (acc, ss) r -> 
      let acc = acc + r * s.(r) - ss + Fenwick.sum tree (s.(r) + 1) m * m in
      Fenwick.add tree s.(r) 1;
      acc, ss + s.(r)
    )
    (0, 0)

let () = printf "%d\n%!" ans
