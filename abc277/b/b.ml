open Core

let n = Scanf.scanf "%d" ident

let s = Array.init n ~f:(fun _ -> Scanf.scanf " %s" ident)

let satisfied s =
  Array.exists ~f:Char.(equal s.[0]) [| 'H'; 'D'; 'C'; 'S' |] &&
  Array.exists ~f:Char.(equal s.[1]) [| 'A'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K' |]

let rec unique acc =  function
  |  [] -> true
  | s :: rest ->
    if List.exists ~f:String.(equal s) acc then false
    else
      unique (s :: acc) rest
let unique = unique []

let satisfied = Array.for_all s ~f:satisfied && Array.to_list s |> unique

let () = print_endline @@ if satisfied then "Yes" else "No"
