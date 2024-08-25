open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let friends = Array.make_matrix ~dimx:(n+1) ~dimy:(n+1) false

let scan_1 _ = scanf " %d" ident
let () =
  for _ = 1 to m do
    let k = scan_1 () in
    let x = Array.init k ~f:scan_1 in
    for i = 0 to k - 1 do
      for j = i + 1 to k - 1 do
        friends.(x.(i)).(x.(j)) <- true
      done
    done
  done

let ans = 
  Iter.(1 -- n) |> Iter.for_all (fun i ->
      Iter.(i + 1 -- n) |> Iter.for_all (fun j ->
          friends.(i).(j)
        )
    )
let ans = if ans then "Yes" else "No"

let () = printf "%s\n%!" ans
