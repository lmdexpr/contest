open Core
open Scanf

let n = scanf " %d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let t = Int.Set.of_array a |> Set.to_array

let ans = Array.map a ~f:(fun x ->
  Array.binary_search t ~compare `First_equal_to x
  |> Option.value_exn
  |> succ
)

let () =
  Array.iter ans ~f:(printf "%d ");
  printf "\n%!"
