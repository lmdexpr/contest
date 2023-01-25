open Core

let n = Scanf.scanf "%d" ident
let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let all = Array.sum (module Int) a ~f:ident
let ten = all / 10

let a = Array.append (Array.copy a) (Array.copy a)

let rec solve ?(l=0) ?(r=1) acc =
  if acc = ten      then true
  else if r = 2 * n then false
  else if acc < ten then solve ~l ~r:(r + 1) (acc + a.(r)) 
  else (* acc > ten_p *)
    let f x = if l + 1 < r then 0 else x in
    let acc = acc - a.(l) + f a.(r) in
    let l = l + 1 in
    let r = r + f 1 in
    solve ~l ~r acc

let yes = all % 10 = 0 && solve a.(0)

let () = printf "%s\n%!" @@ if yes then "Yes" else "No"
