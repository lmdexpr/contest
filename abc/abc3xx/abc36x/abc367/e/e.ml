open Core
open Scanf

let n, k = scanf "%d %Ld" Tuple2.create

let x = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let cycle =
  let b = Array.copy a in
  let rec loop i =
    let c = Array.copy b in
    for i = 0 to n - 1 do
      b.(i) <- c.(x.(i))
    done;
    Array.iter b ~f:(printf "%d "); printf "\n%!";
    let _ = In_channel.(input_line_exn stdin) in
    let i = i + 1 in
    if Array.equal Int.(=) b c then None
    else if Array.equal Int.(=) a b then Some (i - 1) 
    else 
      loop i
  in
  loop 0

let k = Int64.(k % of_int cycle |> to_int_exn)

let ans =
  let b = Array.copy a in
  for _ = 1 to k do
    let c = Array.copy b in
    for i = 0 to n - 1 do
      b.(i) <- c.(x.(i))
    done;
  done;
  b

let () = Array.iter ans ~f:(printf "%d "); printf "\n"
