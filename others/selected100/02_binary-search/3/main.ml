(* https://atcoder.jp/contests/abc077/tasks/arc084_a *)
open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)
let b = Array.init n ~f:(fun _ -> scanf " %d" ident)
let c = Array.init n ~f:(fun _ -> scanf " %d" ident)

let () = Array.sort ~compare a; Array.sort ~compare c

let lower_bound xs x = Array.binary_search xs ~compare `First_greater_than_or_equal_to x |> Option.value ~default:(Array.length xs)
let upper_bound xs x = Array.binary_search xs ~compare `First_strictly_greater_than    x |> Option.value ~default:(Array.length xs)

let ans = Array.fold b ~init:0L ~f:(fun acc b ->
    Int64.(acc + of_int (lower_bound a b) * of_int Int.(n - upper_bound c b))
  )

let () = printf "%Ld\n%!" ans
