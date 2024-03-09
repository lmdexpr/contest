open Core
open Scanf

let n = scanf "%d" Fn.id

let solve i =
  Iter.(1 -- 9)
  |> Iter.filter (fun j -> n % j = 0 && i % (n / j) = 0)
  |> Iter.min
  |> Option.value_map ~default:'-' ~f:Char.(fun j -> of_int_exn (j + 48))

let () =
  for i = 0 to n do
    printf "%c" @@ solve i
  done;
  printf "\n"
