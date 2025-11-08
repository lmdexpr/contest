open Core
open Scanf

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id
let k = scanf " %d" Fn.id

let h = Array.init n ~f:(fun i -> scanf " %d" Fn.id, i) |> SP.of_array

let b = Array.init m ~f:(fun _ -> scanf " %d" Fn.id)
let () = Array.sort ~compare:Int.descending b

let count = Array.fold_until b ~init:(0, h)
  ~f:Continue_or_stop.(fun (ans, h) b ->
    let compare (v, _) b = compare v b in
    match Set.binary_search ~compare h `Last_less_than_or_equal_to b with
    | None   -> Stop ans
    | Some v -> Continue (ans + 1, Set.remove h v)
  )
  ~finish:fst

let ans = if k <= count then "Yes" else "No"

let () = printf "%s\n%!" ans
