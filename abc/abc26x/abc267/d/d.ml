open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %Ld" ident)

let neg_inf = -1000000000000000000L
let init = Array.init (n + 1) ~f:(const neg_inf)
let () = init.(0) <- 0L

let next dp i = function
  | 0 -> 0L
  | j when j > i -> neg_inf
  | j ->
    let i_1 = i - 1 and j_1 = j - 1 in
    Int64.max dp.(j) Int64.(dp.(j_1) + of_int j * a.(i_1))

let next dp i = Array.init (n + 1) ~f:(next dp i)

let ans = Iter.(1 -- n) |> Iter.fold next init

let () = printf "%Ld\n%!" ans.(m)
