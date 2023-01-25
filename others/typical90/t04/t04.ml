open Core

let h, w = Scanf.scanf "%d %d" Tuple2.create

let sum_row = Array.create ~len:h 0
let sum_col = Array.create ~len:w 0

let a =
  Array.init h ~f:(fun i ->
      Array.init w ~f:(fun j ->
          let aij = Scanf.scanf " %d" ident in
          sum_col.(j) <- sum_col.(j) + aij;
          sum_row.(i) <- sum_row.(i) + aij;
          aij
        )
    )

let () =
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      printf "%d " @@ sum_row.(i) + sum_col.(j) - a.(i).(j)
    done;
    Out_channel.(newline stdout)
  done
