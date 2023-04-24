open Core
open Scanf

let n = scanf "%d" ident

let node = Array.init (n+1) ~f:(fun _ -> Int.Set.empty)
let () =
  for _ = 1 to n - 1 do
    let a, b = scanf " %d %d" Tuple2.create in
    node.(a) <- Int.Set.add node.(a) b;
    node.(b) <- Int.Set.add node.(b) a
  done

let degree = Array.map node ~f:Int.Set.length
let () = Array.sort degree ~compare

let yes = Iter.(1 -- (n - 1)) |> Iter.for_all (fun i -> degree.(i) = 1)
let yes = yes && degree.(n) = n - 1

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans

