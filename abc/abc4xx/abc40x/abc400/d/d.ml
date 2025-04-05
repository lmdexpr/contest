open Core
open Scanf

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let h, w = scanf " %d %d" Tuple2.create

let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array 
  |> Array.map ~f:(function
    | '.' -> true
    | '#' -> false
    | _ -> assert false
  )
)

let a, b = scanf " %d %d" Tuple2.create
let c, d = scanf " %d %d" Tuple2.create

let bfs (sx, sy) (gx, gy) =
  let d = [ (1, 0); (-1, 0); (0, 1); (0, -1) ] in
  let visited = Array.make_matrix ~dimx:h ~dimy:w false in
  let rec bfs q =
    match Fqueue.dequeue_exn q with
    | (ans, i, j), _ when i = gx && j = gy -> ans
    | (_,   i, j), q when visited.(i).(j)  -> bfs q
    | (ans, i, j), q -> 
      visited.(i).(j) <- true;
      List.map d ~f:(fun (di, dj) -> i + di, j + dj)
      |> List.filter ~f:(fun (i, j) ->
        i >= 0 && i < h && j >= 0 && j < w
      )
      |> List.fold ~init:q ~f:(fun q (i, j) ->
        Fqueue.enqueue q @@
        if s.(i).(j) then ans, i, j else (ans + 1, i, j)
      )
      |> bfs
  in
  bfs @@ Fqueue.singleton (0, sx, sy)

let ans = bfs (a - 1, b - 1) (c - 1, d - 1)

let () = printf "%d\n%!" ans
