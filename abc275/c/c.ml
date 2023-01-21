open Core
 
let s = In_channel.(input_lines stdin) |> List.to_array

let (let+) x k = Iter.flat_map k x
let (let*) x k = Iter.filter k x

let is_pawn x y = Char.(s.(x).[y] = '#')
let in_0_9 x = 0 <= x && x < 9

let ans = 
  let+ ax = Iter.(0 -- 8)    in let+ ay = Iter.(0 -- 8) in
  let+ bx = Iter.(ax+1 -- 8) in let* by = Iter.(ay -- 8) in
  is_pawn ax ay &&
  is_pawn bx by &&
  let xx = bx - ax and yy = by - ay in
  let cx = bx + yy and cy = by - xx in in_0_9 cx && in_0_9 cy && is_pawn cx cy && 
  let dx = cx - xx and dy = cy - yy in in_0_9 dx && in_0_9 dy && is_pawn dx dy

let () = printf "%d\n%!" @@ Iter.length ans
