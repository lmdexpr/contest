open Core
open Scanf

module PI = struct
  type t = Int64.t * Int64.t
  let compare = Tuple2.compare ~cmp1:Int64.compare ~cmp2:Int64.compare
  let sexp_of_t = Tuple2.sexp_of_t Int64.sexp_of_t Int64.sexp_of_t
  let t_of_sexp = Tuple2.t_of_sexp Int64.t_of_sexp Int64.t_of_sexp
end
module SP = Set.Make(PI)

let n, m = scanf "%d %d" Tuple2.create

let sx, sy = scanf " %Ld %Ld" Tuple2.create

let from_x = Hashtbl.create (module Int64)
let from_y = Hashtbl.create (module Int64)
let () =
  for _ = 0 to n - 1 do
    let x, y = scanf " %Ld %Ld" Tuple2.create in
    Hashtbl.update from_x x ~f:(function
      | None   -> Int64.Set.singleton y
      | Some s -> Set.add s y
    );
    Hashtbl.update from_y y ~f:(function
      | None   -> Int64.Set.singleton x
      | Some s -> Set.add s x
    )
  done

let dc = Array.init m ~f:(fun _ -> scanf " %c %Ld" Tuple2.create)

let compare = Int64.compare

let remove_range set l r =
  let rec go acc set l =
    match Set.binary_search set ~compare `First_greater_than_or_equal_to l with
    | Some x when Int64.(x <= r) -> go Set.(add acc x) Set.(remove set x) x
    | _                          -> acc, set
  in
  go Int64.Set.empty set l

let update acc from axis l r f =
  match Hashtbl.find from axis with
  | None   -> acc
  | Some s ->
    let d, s = remove_range s l r in
    Hashtbl.set from ~key:axis ~data:s;
    Set.union acc SP.(map d ~f)

let x, y, c =
  Array.fold dc 
    ~init:(sx, sy, SP.empty)
    ~f:(fun (x, y, acc) (d, c) -> 
      let fx x = x, y and fy y = x, y in
      match d with
      | 'L' -> let nx = Int64.(x - c) in nx, y, update acc from_y y nx x fx
      | 'R' -> let nx = Int64.(x + c) in nx, y, update acc from_y y x nx fx
      | 'U' -> let ny = Int64.(y + c) in x, ny, update acc from_x x y ny fy
      | 'D' -> let ny = Int64.(y - c) in x, ny, update acc from_x x ny y fy
      | _   -> x, y, acc
    )

let c = Set.length c

let () = printf "%Ld %Ld %d\n%!" x y c
