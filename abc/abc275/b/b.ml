open Core
open Scanf

let a, b, c = scanf "%Ld %Ld %Ld" Tuple3.create
let d, e, f = scanf " %Ld %Ld %Ld" Tuple3.create

let modulo = 998244353L
let( *%) a b = Int64.(a % modulo * (b % modulo) % modulo)
let (-%) a b = Int64.((a - b) % modulo)

let ans = (a *% b *% c) -% (d *% e *% f)

let () = printf "%Ld\n%!" ans
