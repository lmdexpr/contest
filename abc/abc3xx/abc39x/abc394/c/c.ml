open Core
open Scanf

let s = scanf " %s" String.to_array
let n = Array.length s

let rec solve ?(l=None) i =
  if i >= n then ()
  else 
    match s.(i), l with
    | 'W', None   -> solve ~l:(Some i) (i + 1)
    | 'W', Some _ -> solve ~l (i + 1)
    | 'A', None   -> solve (i + 1)
    | 'A', Some j -> 
      s.(j) <- 'A';
      for k = j + 1 to i do
        s.(k) <- 'C'
      done;
      solve (i + 1)
    | _ -> 
      solve (i + 1)

let () =
  solve 0

let () =
  Array.iter s ~f:(printf "%c%!"); printf "\n%!"
