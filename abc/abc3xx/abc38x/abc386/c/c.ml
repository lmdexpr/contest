open Core
open Scanf

let k = scanf "%d" Fn.id

let s = scanf " %s" String.to_list
let t = scanf " %s" String.to_list

let rec solve k = function
  | [], [] -> true
  | [], t  -> List.length t <= k
  | s, []  -> List.length s <= k
  | sh::st, th::tt when Char.(sh = th) -> solve k (st, tt)
  | sh::st, th::tt -> 
    (* F はそのままでは通らなさそう *)
    let k = k - 1 in
    k >= 0 && (solve k (st, th::tt) || solve k (sh::st, tt) || solve k (st, tt))

let ans = if solve k (s, t) then "Yes" else "No"

let () = printf "%s\n%!" ans
