open Core
open Scanf

let n, m, q = scanf "%d %d %d" Tuple3.create

let q = Array.init q ~f:(fun _ -> scanf " %d %d %d %d" @@ fun a b c d -> a-1, b-1, c, d) |> Iter.of_array

let score arr =
  Iter.filter_map (fun (a, b, c, d) -> Option.some_if (arr.(b) - arr.(a) = c) d) q |> Iter.sum

let duplicate_permutation ~len ~f ~elem_max =
  let a = Array.init len ~f:(const 0) in
  let n = Array.length a in
  let rec dfs pos start =
    if pos = n then Iter.singleton @@ f a
    else
      Iter.(start -- elem_max)
      |> Iter.flat_map (fun next -> a.(pos) <- next; dfs (pos + 1) next)
  in
  dfs 0 1

let ans = duplicate_permutation ~len:n ~f:score ~elem_max:m |> Iter.max_exn ~lt:(<)

let () = printf "%d\n%!" ans
