open Core
open Scanf

let q = scanf " %d" Fn.id

let () =
  let stack = ref [] in
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 -> stack := (scanf " %d" Fn.id) :: !stack
  | 2 -> 
      printf "%d\n%!" (match !stack with
        | []      -> 0
        | x :: xs -> stack := xs; x
      )
    | _ -> assert false
  done
