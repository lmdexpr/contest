open Core
open Scanf

let t = scanf "%d" ident

let test_in () = scanf " %d %s" @@ fun _ s -> s

let heads   = String.count ~f:Char.((=) '1')
let has_adj = String.is_substring ~substring:"11"

let solve s =
  match heads s with
  | k when k % 2 = 1 -> -1
  | 2 when has_adj s ->
    (match s with
     | "110" | "011" -> -1
     | "0110"        -> 3
     | _             -> 2)
  | k                -> k / 2

let () =
  for _ = 1 to t do
    test_in () |> solve |> printf "%d\n%!"
  done
