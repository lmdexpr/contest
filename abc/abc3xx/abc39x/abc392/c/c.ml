open Core
open Scanf

let n = scanf " %d" Fn.id

let person_of_number = Array.create ~len:(n + 1) 0
let focus_of_person  = Array.create ~len:(n + 1) 0
let number_of_person = Array.create ~len:(n + 1) 0

let () =
  for i = 1 to n do
    let p = scanf " %d" Fn.id in
    focus_of_person.(i) <- p;
  done;
  for i = 1 to n do
    let q = scanf " %d" Fn.id in
    number_of_person.(i) <- q;
    person_of_number.(q) <- i;
  done

let () = 
  for i = 1 to n do
    printf "%d " number_of_person.(focus_of_person.(person_of_number.(i)));
  done
