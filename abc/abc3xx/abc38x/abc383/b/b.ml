open Core
open Scanf

let h, w, d = scanf "%d %d %d" Tuple3.create

let s = Array.init h ~f:(fun _ -> 
  scanf " %s" String.to_array |> Array.map ~f:Char.((=) '.')
)

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let floor = SP.of_list @@ Array.foldi s ~init:[] ~f:(fun i acc ->
  Array.foldi ~init:acc ~f:(fun j acc c -> if c then (j, i) :: acc else acc)
)

let possible =
  Iter.(product (0 -- (w - 1)) (0 -- (h - 1)))
  |> Iter.filter (fun (x, y) -> s.(y).(x))

let ans =
  possible |> Iter.fold (fun m (sx, sy) ->
    possible |> Iter.fold (fun m (tx, ty) ->
      floor
      |> Set.filter ~f:(fun (x, y) -> 
        abs (sx - x) + abs (sy - y) <= d ||
        abs (tx - x) + abs (ty - y) <= d
      )
      |> Set.length
      |> max m
    ) m
  ) 0

let () = printf "%d\n%!" ans
