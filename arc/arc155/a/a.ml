open Core
open Scanf

let t = scanf "%d" ident

let is_palindrome s =
  let rec loop i j =
    if i >= j then true
    else
      Char.(s.[i] = s.[j]) && loop (i + 1) (j - 1)
  in
  loop 0 (String.length s - 1)

let solve n k s =
  let k = k % (2 * n) in
  let t = String.init k ~f:(fun i -> s.[if (i / n) % 2 = 0 then n - 1 - i % n else i % n]) in
  let yes = is_palindrome (s ^ t) && is_palindrome (t ^ s) in
  let yes = if yes then "Yes" else "No" in
  printf "%s\n%!" yes

let () =
  for _ = 1 to t do
    scanf " %d %d %s" solve
  done
