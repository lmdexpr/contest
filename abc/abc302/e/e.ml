open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

let edge = Array.create ~len:(n + 1) Int.Set.empty

let solve_1 ans u v =
  let connect ans u v =
    let ans = ans - Bool.to_int (Set.is_empty edge.(u)) in
    edge.(u) <- Set.add edge.(u) v;
    ans
  in
  let ans = connect ans u v in
  let ans = connect ans v u in
  ans

let solve_2 ans v =
  let ans =
    Set.fold edge.(v) ~init:ans ~f:(fun ans u ->
        edge.(u) <- Set.remove edge.(u) v;
        ans + Bool.to_int (Set.is_empty edge.(u))
      )
  in
  let ans = ans + Bool.to_int (Set.length edge.(v) > 0) in
  edge.(v) <- Int.Set.empty;
  ans

let solve ans _ =
  match scanf " %d" ident with
  | 1 -> scanf " %d %d" (solve_1 ans)
  | _ -> scanf " %d"    (solve_2 ans)

let () =
  Iter.(1 -- q)
  |> Iter.scan solve n |> Iter.drop 1
  |> Iter.iter (printf "%d\n")
