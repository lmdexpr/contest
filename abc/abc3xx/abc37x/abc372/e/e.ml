open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

let a = Array.init (n+1) ~f:Union_find.create
let g = Array.init (n+1) ~f:Int.Set.singleton

let () =
  for _ = 1 to q do
    match scanf " %d %d %d" Tuple3.create with
    | (1, u, v) -> 
      let pu = Union_find.get a.(u) in
      let pv = Union_find.get a.(v) in
      Union_find.union a.(u) a.(v);
      let root = Union_find.get a.(u) in
      g.(root) <- Set.union g.(root) @@ Set.union g.(pu) g.(pv);
    | (2, v, k) -> 
      let v = Union_find.get a.(v) in
      let n = Set.length g.(v) in
      (match Set.nth g.(v) (n - k) with
      | Some x -> printf "%d\n%!" x
      | None   -> printf "-1\n%!"
      )
    | _ -> assert false
  done
