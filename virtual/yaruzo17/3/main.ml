open Core
open Scanf

let l = scanf "%Ld" Fn.id

let ans =
  Iter.(1 -- 11)
  |> Iter.fold Int64.(fun ans i ->
      let i = Int64.of_int i in
      ans * (l - i) / i
  )
  1L

let () = printf "%Ld\n%!" ans
