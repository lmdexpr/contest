open Core
open Scanf

let _ = scanf "%d" Fn.id
let s = scanf " %s" Fn.id

let idx  c = Char.to_int c - Char.to_int 'a'
let to_c i = Char.of_int_exn (i + Char.to_int 'a')

let q = scanf " %d" Fn.id
let chars = Array.init 26 ~f:Fn.id
let () =
  for _ = 1 to q do
    let c, d = scanf " %c %c" Tuple2.create in
    for i = 0 to 25 do
      if chars.(i) = idx c then chars.(i) <- idx d
    done
  done

let () =
  String.iter s ~f:(fun c -> printf "%c" @@ to_c chars.(idx c));
  printf "\n"
