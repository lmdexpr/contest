open Core

let () =
  Scanf.scanf "%s" ident
  |> String.to_list
  |> List.fold ~init:0 ~f:(fun acc -> function
      | 'v' -> acc + 1
      | 'w' -> acc + 2
      | _   -> acc
    )
  |> printf "%d\n%!"
