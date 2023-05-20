open Core

let k = Scanf.scanf "%d" ident

let () =
  Iter.(Char.to_int 'A' -- Char.to_int 'Z')
  |> Iter.take k
  |> Iter.map Char.of_int_exn
  |> Iter.iter (printf "%c");
  printf "\n%!"
