open Core
open Scanf

let n = scanf " %d" Fn.id
let q = scanf " %d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let qs = Array.create ~len:n Iter.empty
let () =
  for i = 0 to q - 1 do
    let r, x = scanf " %d %d" Tuple2.create in
    qs.(r - 1) <- Iter.snoc qs.(r - 1) (i, x)
  done

let ans = Array.create ~len:q 0
let () =
  let inf = n + 1 in
  let lis = Array.create ~len:n inf in
  let lower_bound, upper_bound =
    let arr_binsearch meth x = 
      Array.binary_search lis ~compare meth x |> Option.value ~default:n
    in
    arr_binsearch `First_greater_than_or_equal_to,
    arr_binsearch `First_strictly_greater_than
  in
  for i = 0 to n - 1 do
    let k = lower_bound a.(i) in
    lis.(k) <- a.(i);
    qs.(i) |> Iter.iter (fun (j, x) -> ans.(j) <- upper_bound x)
  done

let () =
  Array.iter ans ~f:(printf "%d\n")
