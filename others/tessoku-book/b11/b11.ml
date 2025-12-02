open Core
open Scanf

let n = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let q = scanf " %d" Fn.id

let () =
  Array.sort a ~compare;
  for _ = 1 to q do
    let x = scanf " %d" Fn.id in
    Array.binary_search a ~compare `First_greater_than_or_equal_to x
    |> Option.value ~default:n
    |> printf "%d\n%!"
  done
