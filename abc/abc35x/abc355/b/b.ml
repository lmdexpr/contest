open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let a = List.init n ~f:(fun _ -> scanf " %d" Fn.id, true)
let b = List.init m ~f:(fun _ -> scanf " %d" Fn.id, false)

let c = 
  a @ b 
  |> List.sort ~compare:(fun (a, _) (b, _) -> compare a b)
  |> List.map ~f:snd

let rec yes = function
  | true :: true :: _ -> true
  | _ :: xs           -> yes xs
  | []                -> false

let ans = if yes c then "Yes" else "No"

let () = printf "%s\n%!" ans
