open Core
open Scanf

let _l, n1, n2 = scanf "%Ld %d %d" Tuple3.create

let a = List.init n1 ~f:(fun _ -> scanf " %d %Ld" Tuple2.create)
let b = List.init n2 ~f:(fun _ -> scanf " %d %Ld" Tuple2.create)

let rec solve ?(acc=0L) = function
  | (v1, l1)::a, (v2, l2)::b ->
    let acc = if v1 = v2 then Int64.(acc + min l1 l2) else acc in
    let a, b =
      if Int64.(l1 = l2) then a, b
      else if Int64.(l1 < l2) then a, Int64.(v2, l2 - l1) :: b
      else
        Int64.(v1, l1 - l2) :: a, b
    in
    solve ~acc (a, b)
  | _ -> acc

let ans = solve (a, b)

let () = printf "%Ld\n%!" ans
