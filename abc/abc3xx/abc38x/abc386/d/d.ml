open Core
open Scanf

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let _n, m = scanf "%d %d" Tuple2.create

let bs, ws =
  Iter.(1 -- m)
  |> Iter.fold (fun (bs, ws) _ ->
    let x, y, c = scanf " %d %d %c" Tuple3.create in
    match c with
    | 'B' -> Set.add bs (x, y), ws
    | 'W' -> bs, Set.add ws (x, y)
    | _   -> bs, ws
  ) (SP.empty, SP.empty)

let ws = Set.to_array ws

let yes =
  Set.fold bs ~init:true ~f:(fun yes (bx, by) ->
    yes && (
      match
      Array.binary_search ws ~compare:PI.compare 
        `Last_less_than_or_equal_to (bx, by)
      with
      | None   -> true
      | Some i -> 
        let wx, wy = ws.(i) in
        not (wx <= bx && wy <= by)
    )
  )

let ans = if yes then "Yes" else "No"

let () = printf "%s\n%!" ans
