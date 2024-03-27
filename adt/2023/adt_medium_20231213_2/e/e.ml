open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let b = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let dp_a = Array.create ~len:n false
let dp_b = Array.create ~len:n false
let () =
  dp_a.(0) <- true;
  dp_b.(0) <- true;
  for i = 0 to n - 2 do
    if dp_a.(i) then begin
      if abs (a.(i) - a.(i + 1)) <= k then dp_a.(i + 1) <- true;
      if abs (a.(i) - b.(i + 1)) <= k then dp_b.(i + 1) <- true;
    end;
    if dp_b.(i) then begin
      if abs (b.(i) - a.(i + 1)) <= k then dp_a.(i + 1) <- true;
      if abs (b.(i) - b.(i + 1)) <= k then dp_b.(i + 1) <- true;
    end
  done

let yes = dp_a.(n - 1) || dp_b.(n - 1)

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
