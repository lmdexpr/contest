open Core
open Scanf

let n = scanf " %Ld" Fn.id
let m = scanf " %d" Fn.id

let l = Array.init m ~f:(fun _ -> scanf " %Ld %Ld" Tuple2.create)

let () =
  Array.sort l ~compare:Int64.(fun (a1, b1) (a2, b2) -> compare (a1 - b1) (a2 - b2))

let ans, _ =
  Array.fold l ~init:(0L, n) ~f:Int64.(fun (ans, n) (a, b) ->
    if n < a then ans, n
    else (
      let d = a - b in
      let x = (n - a) / d + 1L in
      ans + x, n - x * d
    )
  )

let () = printf "%Ld\n%!" ans
