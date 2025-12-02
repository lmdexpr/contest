open Core
open Scanf

let n = scanf " %d" Fn.id
let x = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let () =
  match Array.binary_search a ~compare `First_equal_to x with
  | None -> invalid_arg "Not found"
  | Some i -> printf "%d\n%!" @@ i + 1
