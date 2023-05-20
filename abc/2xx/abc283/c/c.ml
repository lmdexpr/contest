open Core

let s = Scanf.scanf "%s" ident |> String.to_list

let rec f ?(acc=0) = function
  | '0' :: '0' :: xs | _ :: xs -> f ~acc:(acc+1) xs
  | [] -> acc

let () = printf "%d\n%!" @@ f s
