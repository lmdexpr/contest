open Core
open Scanf

let test x1 x2 x3 =
  let open Int64 in
  let x = x1 + x2 + x3 in
  if not (x % 3L = 0L && x1 % 2L = x2 % 2L && x2 % 2L = x3 % 2L) then -1L
  else
    let a = x / 3L in
    (abs (x1 - a) + abs (x2 - a) + abs (x3 - a)) / 4L

let t = scanf "%d" ident
let () =
  for _ = 1 to t do
    scanf " %Ld %Ld %Ld" test |> printf "%Ld\n%!"
  done
