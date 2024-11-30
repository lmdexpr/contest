open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun i -> i, scanf " %d" Fn.id)
let b = Array.init m ~f:(fun _ -> scanf " %d" Fn.id)

let a =
  let rec go i hd acc = function
    | []                       -> (i, hd) :: acc
    | (j, x) :: xs when hd > x -> go j x ((i, hd) :: acc) xs
    | _ :: xs                  -> go i hd acc xs
  in
  let a = List.of_array a in
  let i, hd = List.hd_exn a in
  let a     = List.tl_exn a in
  go i hd [] a |> Array.of_list

let ans = Array.map b ~f:(fun b ->
  match
    Array.binary_search a ~compare:(fun (_, x) y -> compare x y)
    `Last_less_than_or_equal_to b
  with
  | None   -> -1
  | Some i -> a.(i) |> fst |> succ
)

let () = Array.iter ans ~f:(printf "%d\n")
