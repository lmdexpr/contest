open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let g = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let visit = Array.make_matrix ~dimx:h ~dimy:w false

let rec move i j =
  if visit.(i).(j) then None
  else begin
    visit.(i).(j) <- true;
    match g.(i).(j) with
    | 'U' when i <> 0   -> move (i - 1) j
    | 'D' when i <> h-1 -> move (i + 1) j
    | 'L' when j <> 0   -> move i (j - 1)
    | 'R' when j <> w-1 -> move i (j + 1)
    | _                 -> Some (i, j)
  end

let () =
  match move 0 0 with
  | Some (i, j) -> printf "%d %d\n%!" (i+1) (j+1)
  | None        -> printf "-1\n%!"
