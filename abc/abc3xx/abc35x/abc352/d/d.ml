open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let q = Array.create ~len:n 0
let () =
  for i = 0 to n - 1 do
    let p = scanf " %d" Fn.id in
    q.(p - 1) <- i
  done

let score set = Set.max_elt_exn set - Set.min_elt_exn set

let first =
  Iter.of_array q |> Iter.take k
  |> Iter.to_array |> Int.Set.of_array

let ans, _ =
  Iter.(k -- (n - 1))
  |> Iter.fold
    (fun (ans, acc) i ->
      let acc = Set.remove acc q.(i - k) in
      let acc = Set.add    acc q.(i) in
      min ans (score acc), acc
    )
    (score first, first)

let () = printf "%d\n%!" ans
