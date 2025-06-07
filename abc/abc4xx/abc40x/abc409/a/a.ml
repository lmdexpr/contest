open Core
open Scanf

let n = scanf " %d" Fn.id
let t = scanf " %s" String.to_array
let a = scanf " %s" String.to_array

let yes =
  Iter.(0 -- pred n)
  |> Iter.exists Char.(fun i ->
    t.(i) = 'o' && a.(i) = 'o'
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
