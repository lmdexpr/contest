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

let n, m = scanf " %d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let g = Array.create ~len:(m+1) []
let l = Array.create ~len:(m+1) 0
let () =
  Array.iteri a ~f:(fun i a -> 
    g.(a) <- i :: g.(a);
    l.(a) <- l.(a) + 1;
  )

let ans0 =
  let f = Fenwick.create m in
  Array.fold a ~init:0 ~f:(fun ans i ->
    let ans = ans + Fenwick.sum f (i + 1) m in
    Fenwick.add f i 1;
    ans
  )

let _ =
  printf "%d\n" ans0;

  Iter.(1 -- (m - 1))
  |> Iter.fold
    (fun ans c ->
      let c1, c2 = 
        List.foldi g.(m - c) ~init:(0, 0) ~f:(fun i (c1, c2) v ->
          c1 + v - i,
          c2 + n - 1 - v - (l.(m - c) - 1 - i)
        )
      in
      let ans = ans + c1 - c2 in
      printf "%d\n" ans; ans
    )
    ans0
