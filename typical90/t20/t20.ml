open Core

let a, b, c = Scanf.scanf "%Ld %Ld %Ld" Tuple3.create

let () = print_endline @@ if Int64.(a < pow c b) then "Yes" else "No"
