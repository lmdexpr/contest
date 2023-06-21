open Core
open Scanf

let n = scanf "%d" ident

let f = Array.init n ~f:(const [])
let () =
  for i = 1 to 3 * n do
    let a = scanf " %d" ident - 1 in
    f.(a) <- i :: f.(a)
  done

let f = Array.mapi f ~f:(fun i -> function [_; x; _] -> x, i+1 | _ -> assert false)
let () = Array.sort f ~compare:(fun (x, _) (y, _) -> Int.compare x y)

let () =
  Array.iter f ~f:(fun (_, x) ->
    printf "%d " x
  );
  printf "\n"
