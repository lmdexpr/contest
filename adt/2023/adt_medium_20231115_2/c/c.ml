open Core
open Scanf

let n = scanf "%d" Fn.id
let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id) |> String.Set.of_array

let yes =
  Set.length s = n &&
  Set.for_all s ~f:(fun s ->
    List.exists ['H'; 'D'; 'C'; 'S'] ~f:(Char.equal s.[0]) &&
    List.exists ['A'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'] ~f:(Char.equal s.[1])
  )

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
