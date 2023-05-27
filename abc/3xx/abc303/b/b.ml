open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let a = Array.init m ~f:(fun _ -> Array.init n ~f:(fun _ -> scanf " %d" ident))

let dislike = Array.make_matrix ~dimx:(n+1) ~dimy:(n+1) true
let () =
  Array.iter a ~f:(fun a ->
      for i = 0 to n - 2 do
        dislike.(a.(i)).(a.(i+1)) <- false;
        dislike.(a.(i+1)).(a.(i)) <- false;
      done
    )

let ans =
  Iter.(1 -- n)
  |> Iter.fold
    (fun acc i ->
       Iter.((i + 1) -- n)
       |> Iter.fold
         (fun acc j -> acc + Bool.to_int dislike.(i).(j))
         acc
    )
    0

let () = printf "%d\n%!" ans
