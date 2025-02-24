open Core
open Scanf

let s = scanf " %s" String.to_list

let rec solve stack = function
  | []       -> List.length stack = 0
  | hd :: tl -> 
    match hd, stack with
    | '(', _ | '[', _ | '<', _ -> solve (hd :: stack) tl

    | ')', '(' :: stack -> solve stack tl
    | ']', '[' :: stack -> solve stack tl
    | '>', '<' :: stack -> solve stack tl

    | _ -> false

let yes = solve [] s

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
