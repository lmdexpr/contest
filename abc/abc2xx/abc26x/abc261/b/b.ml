open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %s" String.to_array)

let correct =
  Iter.product 
    Iter.(0 -- (n - 1))
    Iter.(0 -- (n - 1))
  |> Iter.for_all Char.(fun (i, j) ->
    match a.(i).(j) with
    | 'W' -> a.(j).(i) = 'L'
    | 'L' -> a.(j).(i) = 'W'
    | 'D' -> a.(j).(i) = 'D'
    | _   -> true
  )

let () = 
  if correct then
    printf "correct\n%!"
  else
    printf "incorrect\n%!"
