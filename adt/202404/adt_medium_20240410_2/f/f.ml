open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let uf = Array.init (n+1) ~f:Union_find.create

let e = Array.init m ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let ans = 
  Array.fold e ~init:0 ~f:(fun acc (a, b) ->
    if Union_find.same_class uf.(a) uf.(b) then acc + 1
    else (
      Union_find.union uf.(a) uf.(b); acc
    )
  )

let () = printf "%d\n%!" ans
