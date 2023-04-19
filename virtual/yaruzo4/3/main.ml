open Core
open Scanf

let n, k = scanf "%Ld %Ld" Tuple2.create
    
open Int64

let ans = n % k
let ans = min ans (abs @@ ans - k)

let () = printf "%Ld\n%!" ans
