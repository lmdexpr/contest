open Core

let _, q = Scanf.scanf "%d %d" Tuple2.create

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SI = Set.Make(PI)

let query set =
  let t, a, b = Scanf.scanf " %d %d %d" Tuple3.create in
  match t with
  | 1 -> SI.add set (a, b)
  | 2 -> SI.remove set (a, b)
  | _ ->
    let is_ff = SI.mem set (a, b) && SI.mem set (b, a) in
    print_endline @@ if is_ff then "Yes" else "No";
    set

let rec do_ i ~times ~f ~init =
  if i <> times then
    let init = f init in
    do_ (i + 1) ~times ~f ~init

let do_ = do_ 0

let () = do_ ~times:q ~f:query ~init:SI.empty
