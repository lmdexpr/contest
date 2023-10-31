open Core
open Scanf

let s = scanf "%s" String.to_array

let ans =
  Iter.(0 -- 9999)
  |> Iter.map (sprintf "%04d")
  |> Iter.filter (fun i ->
    let to_num c = Char.to_int c - Char.to_int '0' in
    let a = to_num i.[0] in
    let b = to_num i.[1] in
    let c = to_num i.[2] in
    let d = to_num i.[3] in
    Iter.of_array s
    |> Iter.zip_i
    |> Iter.for_all (function
      | i, 'o' -> a = i || b = i || c = i || d = i
      | i, 'x' -> a <> i && b <> i && c <> i && d <> i
      | _ -> true
    )
  )
  |> Iter.length

let () = printf "%d\n%!" ans
