open Core
open Scanf

let s = scanf "%s" String.to_array

let () =
  for i = 0 to Array.length s / 2 - 1 do
    let i = 2 * i in
    Array.swap s i (i + 1)
  done

let () = Array.iter s ~f:(printf "%c"); printf "\n%!"
