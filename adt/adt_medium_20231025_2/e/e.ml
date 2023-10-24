module IntSet = Set.Make(Int)
open Core
open Scanf

let n, x = scanf "%d %d" Tuple2.create

let a = List.init n ~f:(fun _ -> scanf " %d" Fn.id) |> IntSet.of_list

let yes =
  IntSet.exists (fun ai -> IntSet.find_opt (ai + x) a |> Option.is_some) a

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
