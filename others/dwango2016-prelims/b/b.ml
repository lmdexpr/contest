open Core

let n = Scanf.scanf "%d" ident

let k = Array.init (n - 1) ~f:(fun _ -> Scanf.scanf " %d" ident)

let l = Array.init n ~f:(const @@ -1)

let () =
  l.(0) <- k.(0);
  for i = 1 to n - 2 do
    l.(i) <- min k.(i - 1) k.(i)
  done;
  l.(n - 1) <- k.(n - 2)

let () = Array.iter l ~f:(printf "%d "); printf "\n%!"
