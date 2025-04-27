open Core
open Scanf

let t = scanf " %s" String.to_list
let u = scanf " %s" String.to_list

let rec match_ = function
  | _, [] -> true
  | [], _ -> false
  | '?' :: xs, _ :: ys -> match_ (xs, ys)
  | x :: xs, y :: ys   -> Char.(x = y) && match_ (xs, ys)

let rec solve t u =
  if List.length t < List.length u then false
  else
    match_ (t, u) || solve List.(tl_exn t) u

let yes = solve t u

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
