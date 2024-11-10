open Core
open Scanf

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let q = scanf "%d" Fn.id

let () =
  Iter.(1 -- q)
  |> Iter.fold (fun pot _ ->
    match scanf " %d" Fn.id with
    | 1 -> 
      (match Set.min_elt pot with
        | Some (0, b) -> Set.add (Set.remove pot (0, b)) (0, b + 1) 
        | _           -> Set.add pot (0, 1)
      )
    | 2 -> 
      let t = scanf " %d" Fn.id in
      SP.map pot ~f:(fun (a, b) -> a + t, b)
    | 3 -> 
      let h = scanf " %d" Fn.id in
      let a = Set.to_array pot in
      let pot, ans =
        match
        Array.binary_search a ~compare:PI.compare 
          `First_greater_than_or_equal_to (h, 0)
        with
        | None   ->  pot, 0
        | Some i ->
          Iter.(i -- (Array.length a - 1))
          |> Iter.fold (fun (pot, ans) i ->
            let a, b = a.(i) in
            Set.remove pot (a, b), ans + b
          ) (pot, 0)
      in
      printf "%d\n%!" ans;
      pot
    | _ -> assert false
  )
    SP.empty
  |> ignore
