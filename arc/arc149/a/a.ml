open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let (+%) x y = (x + y) % m
let( *%) x y = (x * y) % m

module Memo = struct
  include Memo
  let recursive2 f =
    let memo = Array.make_matrix ~dimx:(n + 1) ~dimy:10 None in
    let rec g x y =
      match memo.(x).(y) with
      | Some v -> v
      | None ->
        let z = f g x y in
        memo.(x).(y) <- Some z; z
    in g
end

let x self = function
  | 0,   _ -> 0
  | 1,   v -> v % m
  | len, v -> self (len - 1) v *% 10 +% v
let x = Memo.recursive2 @@ fun self -> Tuple2.curry (x self)

let ans =
  Iter.(n --^ 1)
  |> Iter.flat_map (fun len ->
      Iter.(1 -- 9)
      |> Iter.filter (fun v -> x len v = 0)
      |> Iter.map (fun v -> len, v)
    )
  |> Iter.max ~lt:(fun (l1, v1) (l2, v2) -> l1 < l2 || (l1 = l2 && v1 < v2))

let () =
  match ans with
  | None -> printf "-1\n%!"
  | Some (len, v) ->
    for _ = 1 to len do
      printf "%d" v
    done;
    printf "\n%!"
