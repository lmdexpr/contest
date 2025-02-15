open Core
open Scanf

let _n, m = scanf " %d %d" Tuple2.create

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let es = Array.init m ~f:(fun _ -> scanf " %d %d" @@ fun u v -> min u v, max u v)

let _, ans =
  Array.fold es ~init:(SP.empty, 0) ~f:(fun (s, ans) (u, v) ->
    if Set.mem s (u, v) then 
      s, ans + 1
    else 
      Set.add s (u, v), ans + Bool.to_int (u = v)
  )

let () = printf "%d\n%!" ans
