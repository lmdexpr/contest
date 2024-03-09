open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let s = scanf " %s" ident |> String.to_array

let rec solve ?(k=k) i =
  if i >= n then ()
  else
    match s.(i) with
    | 'o' when k > 0 -> solve ~k:(k-1) (i+1)
    | _ ->
      s.(i) <- 'x';
      solve ~k (i+1)

let () = solve 0; Array.iter s ~f:(printf "%c"); printf "\n%!"
