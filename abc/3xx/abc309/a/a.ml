open Core
open Scanf

let a, b = scanf "%d %d" Tuple2.create

let yes = List.mem [ 1; 2; 4; 5; 7; 8 ] a ~equal:(=) && b = a + 1

let ans = if yes then "Yes" else "No"
let () = printf "%s\n%!" ans
