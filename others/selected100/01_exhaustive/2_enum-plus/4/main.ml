(* https://atcoder.jp/contests/s8pc-6/tasks/s8pc_6_b *)

open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(const 0L)
let b = Array.init n ~f:(const 0L)
let () =
  for i = 0 to n - 1 do
    scanf " %Ld %Ld" (fun x y -> a.(i) <- x; b.(i) <- y)
  done

let ans =
  Array.fold a ~init:1_000_000_000_000L ~f:(fun acc entrance ->
      Array.fold b ~init:acc ~f:(fun acc exit ->
          Iter.(0 -- (n - 1))
          |> Iter.map Int64.(fun i ->
              abs (entrance - a.(i)) + abs (a.(i) - b.(i)) + abs (b.(i) - exit)
            )
          |> Iter.fold Int64.(+) 0L
          |> Int64.min acc
        )
    )

let () = printf "%Ld\n%!" ans
