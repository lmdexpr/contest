open Core
open Scanf

let m = scanf "%d" Fn.id

let threes =
  Iter.(10 --^ 0)
  |> Iter.map (fun i -> Int.pow 3 i, i)

let rec solve m =
  if m <= 0 then []
  else
    let (x, a) =
      threes
      |> Iter.find_pred_exn (fun (x, _) -> x <= m)
    in
    a :: solve (m - x)

let a = solve m
let n = List.length a

let () = 
  printf "%d\n%!" n;
  List.iter a ~f:(printf "%d "); printf "\n%!"
