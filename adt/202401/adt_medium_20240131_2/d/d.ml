open Core
open Scanf

let n = scanf "%d" Fn.id

let suit = function
  | 'H' -> 1 
  | 'D' -> 2
  | 'C' -> 3 
  | 'S' -> 4 
  | _   -> -1

let rank = function
  | 'A' -> 1
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | s   ->
    let r = Char.to_int s - Char.to_int '0' in if 1 < r && r < 10 then r else -1

let s = Array.init n ~f:(fun _ -> scanf " %s" @@ fun s -> suit s.[0], rank s.[1])
let t = Array.map s ~f:(fun (s, r) -> s * 100 + r) |> Int.Set.of_array

let yes =
  Array.for_all s ~f:(fun (s, r) -> s > 0 && r > 0) &&
  Set.length t = n

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
