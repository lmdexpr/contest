open Core
open Scanf

let n = scanf "%d" ident
let n = n % 30

let cards = [| 1; 2; 3; 4; 5; 6 |]

let rec solve i =
  if i >= n then ()
  else begin
    Array.swap cards (i % 5) (i % 5 + 1);
    solve (i + 1)
  end

let () =
  solve 0;
  Array.iter cards ~f:(printf "%d");
  printf "\n%!"
