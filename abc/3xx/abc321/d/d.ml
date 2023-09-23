open Core
open Scanf

let n, m, p = scanf "%d %d %Ld" Tuple3.create

let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)
let b = Array.init m ~f:(fun _ -> scanf " %Ld" Fn.id)
let () = Array.sort b ~compare:Int64.compare

let b_sum =
  Array.append [| 0L |] b
  |> Array.folding_map ~init:0L ~f:Int64.(fun acc x -> acc + x, acc + x)

let ans =
  let open Int64 in
  Array.fold a ~init:0L ~f:(fun acc x ->
    let i = Array.binary_search ~compare b `First_greater_than_or_equal_to (p - x) in
    let i = Option.value ~default:m i in
    acc + x * of_int i + b_sum.(i) + p * of_int Int.(m - i)
  )

let () = printf "%Ld\n%!" ans
