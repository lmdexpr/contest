open Core

let n = Scanf.scanf "%d" ident
let s = Scanf.scanf " %s" ident

let rec solve ?(acc=0) l r =
  if r = n then acc
  else if Char.(s.[l] = s.[r]) then solve ~acc l (r + 1)
  else
    let acc = acc + (n - r) * (r - l) in
    solve ~acc r (r + 1)

let () = printf "%d\n%!" @@ solve 0 1
