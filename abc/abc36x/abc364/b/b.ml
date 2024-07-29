open Core
open Scanf

let h, w = scanf "%d %d" Tuple2.create

let si, sj = scanf " %d %d" Tuple2.create

let c = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

let x = scanf " %s" Fn.id

let () = 
  let gi, gj = ref si, ref sj in
  let go x y =
    Char.(c.(x - 1).(y - 1) <> '#')
  in
  String.iter x ~f:(function
    | 'U' -> if !gi > 1 && go (!gi - 1) !gj then decr gi
    | 'D' -> if !gi < h && go (!gi + 1) !gj then incr gi
    | 'L' -> if !gj > 1 && go !gi (!gj - 1) then decr gj
    | 'R' -> if !gj < w && go !gi (!gj + 1) then incr gj
    | _   -> assert false
  );
  printf "%d %d\n" !gi !gj;
