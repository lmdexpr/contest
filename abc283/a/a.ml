open Core

let a, b = Scanf.scanf "%Ld %Ld" Tuple2.create

let () = printf "%Ld\n%!" @@ Int64.pow a b
