open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.make_matrix ~dimx:n ~dimy:n (-1)
let d = [| (0, 1); (1, 0); (0, -1); (-1, 0) |]
let inside i j = 0 <= i && i < n && 0 <= j && j < n

let rec next ?(c=0) (i, j) di =
  if c > 4 then None
  else
    let dx, dy = d.(di) in
    let ni, nj = i + dx, j + dy in
    if inside ni nj && a.(ni).(nj) = -1 then Some di
    else
      next ~c:(c + 1) (i, j) ((di + 1) % 4)

let rec solve x (i, j) di = 
  a.(i).(j) <- x;
  next (i, j) di
  |> Option.iter ~f:(fun di ->
    let dx, dy = d.(di) in
    solve (x + 1) (i + dx, j + dy) di
  ) 

let () =
  solve 1 (0, 0) 0;
  a.((n + 1) / 2 - 1).((n + 1) / 2 - 1) <- 0;
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if a.(i).(j) = 0 then printf "T "
      else
        printf "%d " a.(i).(j)
    done;
    printf "\n"
  done
