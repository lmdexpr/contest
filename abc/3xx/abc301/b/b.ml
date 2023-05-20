open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let rec solve i =
  if i < n - 1 then begin
    printf "%d " a.(i);
    let iter = 
      if a.(i) - a.(i + 1) > 1 then Iter.((a.(i) - 1) --^ (a.(i + 1) + 1))
      else if a.(i) - a.(i + 1) < -1 then Iter.((a.(i) + 1) -- (a.(i + 1) - 1))
      else
        Iter.empty
    in
    Iter.iter (printf "%d ") iter;
    solve @@ i + 1
  end

let () =
  solve 0;
  printf "%d\n" a.(n - 1)
