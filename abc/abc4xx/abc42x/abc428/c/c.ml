open Core
open Scanf

let q = scanf " %d" Fn.id

let delta_of_paren = function
  | '(' -> 1
  | ')' -> -1
  | _   -> 0

let ans yes = printf "%s\n%!" @@ if yes then "Yes" else "No"

let hd = List.hd_exn
let tl = List.tl_exn

let rec solve i a b =
  if q < i then ()
  else (
    let a, b =
      match scanf " %d" Fn.id with
      | 1 ->
        let d = scanf " %c" delta_of_paren in
        let na = hd a + d in
        na :: a, min na (hd b) :: b
      | _ -> tl a, tl b
    in
    ans (hd a = 0 && hd b = 0);
    solve (succ i) a b
  )

let () = solve 1 [ 0 ] [ 0 ]
