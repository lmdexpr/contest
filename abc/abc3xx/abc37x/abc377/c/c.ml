open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let knights = Array.init m ~f:(fun _ -> scanf " %d %d" Tuple2.create)

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let coans = Array.fold knights ~init:SP.empty ~f:(fun acc (a, b) ->
  [
    (a, b);
    (a + 2, b + 1);
    (a + 2, b - 1);
    (a - 2, b + 1);
    (a - 2, b - 1);
    (a + 1, b + 2);
    (a + 1, b - 2);
    (a - 1, b + 2);
    (a - 1, b - 2);
  ]
  |> List.filter ~f:(fun (a, b) -> 1 <= a && a <= n && 1 <= b && b <= n)
  |> SP.of_list
  |> Set.union acc
)
  |> Set.length

let ans = Int64.(of_int n * of_int n - of_int coans)

let () = printf "%Ld\n%!" ans
