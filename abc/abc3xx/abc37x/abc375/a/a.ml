open Core
open Scanf

let n = scanf  "%d" Fn.id
let s = scanf " %s" String.to_array |> Array.map ~f:(function '#' -> true | _ -> false)

let ans = 
  Iter.(0 -- (n - 3))
  |> Iter.filter (fun i -> s.(i) && not s.(i + 1) && s.(i + 2))
  |> Iter.length

let () = printf "%d\n%!" ans
