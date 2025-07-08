open Core
open Scanf

let yes n a =
  Array.sort a ~compare:Int64.(fun x y -> compare (abs x) (abs y));
  Iter.(0 -- (n - 3)) |> Iter.for_all (fun i ->
    let i'  = Int.succ i in
    let i'' = Int.succ i' in
    Int64.(a.(i) * a.(i'') = a.(i') * a.(i'))
  )

let yes n a =
  let pos_count = Array.count a ~f:Int64.(fun x -> x = a.(0)) in
  let neg_count = Array.count a ~f:Int64.(fun x -> x = - a.(0)) in
  (pos_count + neg_count = n && min pos_count neg_count = n / 2) ||
  yes n a

let t = scanf " %d" Fn.id

let () =
  for _ = 1 to t do
    let n = scanf " %d" Fn.id in
    let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id) in
    printf "%s\n%!" @@ if yes n a then "Yes" else "No"
  done
