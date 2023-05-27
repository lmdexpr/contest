open Core
open Scanf

let _, m = scanf "%d %d" Tuple2.create
let h, k = scanf " %d %d" Tuple2.create

let d =
  scanf " %s" String.to_list
  |> List.map ~f:(function
      | 'R' -> 1,  0
      | 'L' -> -1, 0
      | 'U' -> 0,  1
      | _   -> 0, -1
    )

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let item =
  Array.init m ~f:(fun _ -> scanf " %d %d" Tuple2.create)
  |> SP.of_array

let step item hp (x, y) (dx, dy) =
  let x = x + dx in
  let y = y + dy in
  let hp = hp - 1 in
  if hp < 0 then None
  else
    let item, hp =
      if hp < k && SP.mem item (x, y)
      then SP.remove item (x, y), k
      else item, hp
    in
    Some (item, hp, x, y)

let rec yes = function
  | None                  -> const false
  | Some (item, hp, x, y) ->
    function
    | []      -> true
    | d :: ds -> yes (step item hp (x, y) d) ds

let ans = if yes (Some (item, h, 0, 0)) d then "Yes" else "No"

let () = printf "%s\n%!" ans
