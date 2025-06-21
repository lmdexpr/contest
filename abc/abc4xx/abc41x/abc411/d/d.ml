open Core
open Scanf

let n = scanf " %d" Fn.id
let q = scanf " %d" Fn.id

let pc = Array.create ~len:n Iter.empty
let server = ref Iter.empty

let () =
  for _ = 1 to q do
    scanf " %d" @@ function
    | 1 ->
      scanf " %d" @@ fun p ->
      pc.(p - 1) <- !server;
    | 2 ->
      scanf " %d %s" @@ fun p s ->
      pc.(p - 1) <- Iter.snoc pc.(p - 1) s;
    | _ ->
      scanf " %d" @@ fun p ->
      server := pc.(p - 1);
  done;
  Iter.iter (printf "%s") !server;
  printf "\n%!"
