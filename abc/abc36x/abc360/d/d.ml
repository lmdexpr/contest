open Core
open Scanf

let n, t = scanf "%d %d" Tuple2.create
let s = scanf " %s" Fn.id
let x = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let pos, neg =
  Iter.(0 -- (n - 1))
  |> Fn.flip Iter.fold ([], []) (fun (pos, neg) i ->
    match s.[i] with
    | '1' -> x.(i) :: pos, neg
    |  _  -> pos, x.(i) :: neg
  )

let pos = List.sort pos ~compare
let neg = List.sort neg ~compare |> List.to_array

let ans =
  let n = Array.length neg in
  let binary_search = Array.binary_search neg ~compare in
  List.fold pos ~init:0 ~f:(fun ans pos ->
    let l = binary_search `First_greater_than_or_equal_to pos in
    let r = binary_search `Last_less_than_or_equal_to (pos + 2 * t) in
    match l, r with
    | Some l, None when l <= pos + 2 * t -> ans + n - l + 1
    | Some l, Some r                     -> ans + r - l + 1
    | _                                  -> ans
  )

let () = printf "%d\n%!" ans
