open Core
open Scanf

let n = scanf "%d" Fn.id

let ans =
  Iter.(n -- 919)
  |> Iter.find_pred_exn (fun x ->
    let s = Int.to_string x in
    let to_i c = Char.to_int c - Char.to_int '0' in
    to_i s.[0] * to_i s.[1] = to_i s.[2]
  )

let () = printf "%d\n%!" ans
