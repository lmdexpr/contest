open Core
open Scanf

let rec combinations ?(acc=Iter.empty) = function
  | 0 -> const @@ Iter.singleton Iter.empty
  | k -> generate acc k
and
  generate acc len iter = match Iter.head iter, Iter.drop 1 iter with
  | None, _     -> acc
  | Some hd, tl -> 
    let acc = combinations (len - 1) tl |> Iter.fold (fun acc cs -> Iter.snoc acc (Iter.cons hd cs)) acc in
    combinations ~acc len tl

let a, b, c = scanf "%d %d %d" Tuple3.create
let d, e    = scanf " %d %d"   Tuple2.create

let input = Iter.of_list [ a, "A"; b, "B"; c, "C"; d, "D"; e, "E" ]

let ans =
  Iter.of_list [
    combinations 1 input;
    combinations 2 input;
    combinations 3 input;
    combinations 4 input;
    combinations 5 input;
  ]
  |> Iter.flatten
  |> Iter.map (fun iter ->
    Iter.fold (fun acc (x, _) -> acc + x) 0 iter,
    Iter.to_string ~sep:"" snd iter
  )
  |> Iter.sort ~cmp:(Tuple2.compare ~cmp1:Int.descending ~cmp2:String.compare)
  |> Iter.uniq

let () = 
  Iter.iter (fun (_, s) -> printf "%s\n" s) ans
