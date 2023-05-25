open Core
open Scanf

let n = scanf "%d" ident
let s = scanf " %s" ident
let t = scanf " %s" ident

let sa = Array.init 26 ~f:(const 0)
let ta = Array.init 26 ~f:(const 0)
let () =
  for i = 0 to n - 1 do
    let ord s = Char.to_int s.[i] - Char.to_int 'a' in
    sa.(ord s) <- sa.(ord s) + 1;
    ta.(ord t) <- ta.(ord t) + 1
  done;
  for i = 0 to 25 do
    if sa.(i) <> ta.(i) then (printf "-1\n%!"; exit 0)
  done

let rec greedy i j =
  if j < 0 then i + 1
  else if Char.(s.[i] = t.[j]) then greedy (i - 1) (j - 1)
  else
    greedy i (j - 1)

let () =
  greedy (n - 1) (n - 1)
  |> printf "%d\n%!"
