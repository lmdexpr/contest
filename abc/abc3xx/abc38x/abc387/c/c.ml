open Core
open Scanf

let l, r = scanf "%Ld %Ld" Tuple2.create 

let array_of_int64 x =
  Int64.to_string x
  |> String.to_array
  |> Array.map ~f:(fun c -> Int64.of_int @@ Char.to_int c - Char.to_int '0')

let snake_numbers_under x =
  let d = array_of_int64 x in
  let n = Array.length d in
  let r i = Int64.of_int (n - 1 - i) in
  Iter.(1 -- n)
  |> Fn.flip Iter.fold_while 0L
    Int64.(fun acc i ->
    if Int.(i = n) then acc + 1L, `Stop
    else
      acc + d.(0) ** r i * min d.(0) d.(i),
      if d.(0) <= d.(i) then `Stop else `Continue
    )
  |> fun res ->
  Iter.(0 -- (n - 1))
  |> Fn.flip Iter.fold res
    (fun res i ->
      let mx = if i <> 0 then 9 else Int64.to_int_exn d.(0) - 1 in
      Iter.(1 -- mx)
      |> Iter.fold Int64.(fun res j -> res + of_int j ** r i) res
    )

let ans = Int64.(snake_numbers_under r - snake_numbers_under (l - 1L))

let () = printf "%Ld\n%!" ans
