open Core
open Scanf

let _ = scanf "%d" ident
let a = scanf " %s" String.to_array |> Array.map ~f:Char.(fun c -> to_int c - to_int '0')
let b = scanf " %s" String.to_array |> Array.map ~f:Char.(fun c -> to_int c - to_int '0')

let modulo = 998244353
let( +%) a b = (a + b) % modulo
let( *%) a b = (a * b) % modulo

let x, y =
  Array.zip_exn a b
  |> Array.fold ~init:(0, 0) ~f:(fun (x, y) (a, b) ->
      x *% 10 +% min a b, y *% 10 +% max a b
    )

let () = printf "%d\n%!" (x *% y)
