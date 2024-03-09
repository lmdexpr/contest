open Core
open Scanf

let _n = scanf "%d" ident
let s = scanf " %s" String.to_list

let print_stack stack =
  stack |> List.rev |> List.iter ~f:(Iter.iter (printf "%c"))

let rec solve ?(stack=[]) = function
  | []          -> print_stack stack
  | '(' :: rest -> solve ~stack:(Iter.singleton '(' :: stack) rest
  | ')' :: rest ->
    begin match stack with
    | []         -> printf ")"; solve rest
    | p :: stack ->
      match Iter.head p with
      | Some '(' -> solve ~stack rest
      | _        -> print_stack (Iter.snoc p ')' :: stack); solve rest
    end
  | c :: rest ->
    match stack with
    | []         -> printf "%c" c; solve rest
    | p :: stack -> solve ~stack:(Iter.snoc p c :: stack) rest

let () = solve s; printf "\n"
