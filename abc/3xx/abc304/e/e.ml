open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let uf = Array.init (n+1) ~f:Union_find.create
let () =
  for _ = 1 to m do
    scanf " %d %d" @@ fun u v -> Union_find.union uf.(u) uf.(v)
  done

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)
let set =
  Iter.(1 -- scanf " %d" ident)
  |> Iter.fold (fun set _ ->
      let u, v = scanf " %d %d" Tuple2.create in
      let u = Union_find.get uf.(u) and v = Union_find.get uf.(v) in
      SP.add set (u, v)
    )
    SP.empty

let () =
  scanf " %d" ident 
  |> Array.init ~f:(fun _ -> scanf " %d %d" Tuple2.create)
  |> Array.iter ~f:(fun (p, q) ->
      let p = Union_find.get uf.(p) and q = Union_find.get uf.(q) in
      let no = SP.mem set (p, q) || SP.mem set (q, p) in
      let ans = if not no then "Yes" else "No" in
      printf "%s\n" ans
    )
