open Core
open Scanf

let n = ref 0
let () =
  for i = 1 to 12 do
    let j = scanf "%s\n" String.length in
    if i = j then
      incr n
  done

let ans = !n

let () = printf "%d\n%!" ans
