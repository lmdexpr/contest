open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" ident) |> Iter.of_array

let m = scanf " %d" ident
let b = Array.init m ~f:(fun _ -> scanf " %d" ident)

let x = scanf " %d" ident

let mochi = Array.init (x+1) ~f:(const false)
let () = Array.iter b ~f:(fun b -> mochi.(b) <- true)

let dp = Array.init (x+1) ~f:(const false)

let () =
  dp.(0) <- true;
  for i = 1 to x do
    dp.(i) <- not mochi.(i) && (
        a
        |> Iter.filter (fun a -> i - a >= 0)
        |> Iter.map    (fun a -> dp.(i - a))
        |> Iter.exists ident
      )
  done

let ans = if dp.(x) then "Yes" else "No"

let () = printf "%s\n%!" ans
