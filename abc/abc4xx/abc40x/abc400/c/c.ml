open Core
open Scanf

let n = scanf " %Ld" Fn.id

let ans =
  Iter.(1 -- 60)
  |> Iter.scan Int64.(fun acc _ -> acc * 2L) 1L
  |> Iter.drop 1
  |> Iter.fold Int64.(fun acc a -> 
    let b =
      n / a |> Int64.to_float |> Float.sqrt |> Int64.of_float
    in
    let b =
      if a * (b + 1L) * (b + 1L) <= n then b + 1L
      else 
        b
    in
    acc + if b % 2L = 0L then b / 2L else (b + 1L) / 2L
  ) 0L

let () = printf "%Ld\n%!" ans
