open Core
open Scanf

let n = scanf "%d" Fn.id

let pi = "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"

let () =
  printf "3.";
  for i = 0 to n - 1 do
    printf "%c" pi.[i]
  done;
  printf "\n"
