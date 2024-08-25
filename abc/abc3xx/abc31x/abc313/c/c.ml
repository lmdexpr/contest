open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let () = Array.sort a ~compare

let sum = Array.sum (module Int) a ~f:Fn.id

let b = Array.init n ~f:(const @@ sum / n)
let () =
  for i = 0 to sum mod n - 1 do
    b.(n - 1 - i) <- b.(n - 1 - i) + 1
  done

let ans =
  Iter.(0 -- (n - 1))
  |> Iter.fold (fun acc i -> acc + abs (a.(i) - b.(i))) 0

let ans = ans / 2

let () = printf "%d\n%!" ans
