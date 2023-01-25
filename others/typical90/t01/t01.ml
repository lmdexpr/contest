open Core

let rec binsearch ~ok left right =
  if abs (right - left) <= 1 then right
  else
    let mid = (right + left) / 2 in
    let (left, right) = if ok mid then (left, mid) else (mid, right) in
    binsearch ~ok left right

let n, l = Scanf.scanf "%d %d" Tuple2.create

let k = Scanf.scanf " %d" ident

let a = l :: List.init n ~f:(fun _ -> Scanf.scanf " %d" ident) |> List.rev

let rec count_of_pieces ?(bias=0) ?(c=0) by = function
  | []      -> c
  | x :: xs ->
    if x - bias < by then count_of_pieces ~bias ~c by xs
    else 
      count_of_pieces ~bias:x ~c:(c+1) by xs

let ok m = count_of_pieces m a >= k + 1 
let () =
  printf "%d\n%!" @@ binsearch ~ok l (-1)
