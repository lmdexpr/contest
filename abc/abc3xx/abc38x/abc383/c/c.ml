open Core
open Scanf

let h, w, d = scanf "%d %d %d" Tuple3.create

let s = Array.init h ~f:(fun _ -> scanf " %s" String.to_array)

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let dist = Array.make_matrix ~dimx:h ~dimy:w Int.max_value

let bfs (sx, sy) =
  let rec bfs q =
    match Fqueue.dequeue q with
    | None                            -> ()
    | Some ((e, _, _), _) when d < e  -> ()
    | Some ((d, x, y), q) when dist.(y).(x) <= d -> bfs q
    | Some ((d, x, y), q) ->
      dist.(y).(x) <- d;
      [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
      |> List.map ~f:(fun (dx, dy) -> x + dx, y + dy)
      |> List.filter ~f:(fun (x, y) -> 
        0 <= x && x < w && 0 <= y && y < h && Char.(s.(y).(x) = '.') &&
        d < dist.(y).(x)
      )
      |> List.fold ~init:q ~f:(fun q (x, y) -> Fqueue.enqueue q (d + 1, x, y))
      |> bfs
  in
  bfs @@ Fqueue.singleton (0, sx, sy)

let () =
  Array.iteri s ~f:(fun i s ->
    Array.iteri s ~f:(fun j c -> if Char.(c = 'H') then bfs (j, i))
  )

let ans = 
  Array.fold dist ~init:0 ~f:(fun init dist ->
    Array.fold dist ~init ~f:(fun ans dist -> ans + Bool.to_int (dist <= d))
  )

let () = printf "%d\n%!" ans
