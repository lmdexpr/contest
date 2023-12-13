open Core
open Scanf

let n, d = scanf "%d %d" Tuple2.create

let s = Array.init n ~f:(fun _ -> scanf " %s" String.to_array |> Array.map ~f:Char.((=) 'o'))

let ans =
  Iter.(0 -- (d - 1))
  |> Iter.map (fun i ->
    Iter.(i -- (d - 1))
    |> Iter.take_while (fun j -> Iter.(0 -- (n - 1)) |> Iter.for_all (fun k -> s.(k).(j)))
    |> Iter.length
  )
  |> Iter.max_exn ~lt:(<)

let () = printf "%d\n%!" ans
