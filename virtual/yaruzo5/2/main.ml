open Core
open Scanf

let x = scanf "%Ld" ident

open Int64

let deposit = ref 100L
let () =
  for year = 1 to 3760 do
    deposit := !deposit + !deposit / 100L;
    if !deposit >= x then begin
      printf "%d\n%!" year;
      exit 0
    end
  done

