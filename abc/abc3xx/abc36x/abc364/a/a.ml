open Core
open Scanf

let n = scanf "%d" Fn.id

let () =
  let flag = ref false in
  for _ = 1 to n - 1 do
    match scanf " %s" Fn.id with
    | "sweet" -> 
      if !flag then (
        printf "No\n"; exit 0
      ) else
        flag := true
    | _ -> 
      flag := false
  done;
  let _ = scanf " %s" Fn.id in
  printf "Yes\n"
