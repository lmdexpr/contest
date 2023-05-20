open Core
open Scanf

let n, m, d = scanf "%d %d %Ld" Tuple3.create

let a = Array.init n ~f:(fun _ -> scanf " %Ld" ident)
let b = Array.init m ~f:(fun _ -> scanf " %Ld" ident)

let compare = Int64.compare

let () = Array.sort a ~compare; Array.sort b ~compare

let _n, a, _m, b = if n < m then n, a, m, b else m, b, n, a

let ans =
  Array.map a ~f:(fun a ->
      match
        Array.binary_search ~compare b `First_greater_than_or_equal_to Int64.(a - d),
        Array.binary_search ~compare b `Last_less_than_or_equal_to Int64.(a + d)
      with
      | None, None | Some _, None | None, Some _ -> -1L
      | Some i, Some j ->
        if j < i then -1L
        else
          Int64.(a + b.(j))
    )
  |> Array.fold ~init:(-1L) ~f:Int64.max

let () = printf "%Ld\n%!" ans
