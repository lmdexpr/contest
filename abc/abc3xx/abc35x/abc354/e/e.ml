open Core
open Scanf

let n = scanf "%d" Fn.id
let cards = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let is_pair i j = 
  let a, b = cards.(i) and c, d = cards.(j) in 
  a = c || b = d

let dp = Array.create ~len:(1 lsl n) (-1)
let () =
  dp.(0) <- 0;
  for i = 1 to 1 lsl n - 1 do
    dp.(i) <- 
      Iter.(0 -- (n - 1)) |> Iter.exists (fun j ->
      Iter.((j + 1) -- (n - 1)) |> Iter.exists (fun k ->
        i lsr j land 1 = 1 && i lsr k land 1 = 1 &&
        is_pair j k && dp.(i lxor (1 lsl j) lxor (1 lsl k)) = 0
      ))
      |> Bool.to_int
  done

let ans = if dp.(1 lsl n - 1) = 1 then "Takahashi" else "Aoki"

let () = printf "%s\n%!" ans
