open Core
open Scanf

let n = scanf "%d" ident
let s = scanf " %s" ident

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let () =
  String.fold s ~init:(SP.singleton (n, n), n, n) ~f:(fun (set, x, y) d ->
      let x, y = 
        match d with
        | 'R' -> x + 1, y
        | 'L' -> x - 1, y
        | 'U' -> x, y + 1
        | 'D' -> x, y - 1
        | _ -> (x, y)
      in
      if SP.mem set (x, y) then (printf "Yes\n%!"; exit 0);
      SP.add set (x, y), x, y
    )
  |> ignore

let () = printf "No\n%!"
