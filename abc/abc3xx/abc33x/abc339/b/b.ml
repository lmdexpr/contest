open Core
open Scanf

let h, w, n = scanf "%d %d %d" Tuple3.create

let ans =
  let ans = Array.make_matrix ~dimx:h ~dimy:w false in
  let dir = [| (-1, 0); (0, 1); (1, 0); (0, -1) |] in
  let rec solve d i j n =
    if n <= 0 then ans
    else begin
      let d = if ans.(i).(j) then d - 1 else d + 1 in
      let d = (d + 4) % 4 in
      let di, dj = dir.(d) in
      ans.(i).(j) <- not ans.(i).(j);
      let i, j = (i + di + h) % h, (j + dj + w) % w in
      solve d i j (n - 1)
    end
  in
  solve 0 0 0 n

let () =
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      printf "%c" (if ans.(i).(j) then '#' else '.')
    done;
    printf "\n"
  done
