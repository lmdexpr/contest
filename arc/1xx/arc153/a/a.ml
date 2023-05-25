open Core

let n = Scanf.scanf "%d" ident

let to_bequtiful n =
  let s = Int.to_string n in
  let s = [ s.[0]; s.[0]; s.[1]; s.[2]; s.[3]; s.[3]; s.[4]; s.[5]; s.[4] ] in
  String.of_char_list s |> Int.of_string

let () =
  Iter.(100000 -- 999999)
  |> Iter.drop (n - 1)
  |> Iter.head_exn
  |> to_bequtiful
  |> printf "%d\n%!"
