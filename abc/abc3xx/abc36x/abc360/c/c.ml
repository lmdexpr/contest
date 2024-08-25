open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)
let w = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let box = Array.create ~len:n SP.empty
let () =
  for i = 0 to n - 1 do
    let a = a.(i) in
    box.(a - 1) <- Set.add box.(a - 1) (w.(i), i)
  done

let ans =
  Array.fold box ~init:0 ~f:(fun acc box ->
    let max = 
      Set.max_elt box
      |> Option.value_map ~default:0 ~f:fst
    in
    Set.fold box ~init:acc ~f:(fun acc (w, _) -> acc + w) - max
  )

let () = printf "%d\n%!" ans
