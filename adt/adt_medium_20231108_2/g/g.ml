open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let uf     = Array.init (n+1) ~f:Union_find.create
let degree = Array.create ~len:(n+1) 0

let exit () = printf "No\n%!" ; exit 0

let () =
  for _ = 1 to m do
    let a, b = scanf " %d %d" Tuple2.create in

    if Union_find.same_class uf.(a) uf.(b) then exit ()
    else
      Union_find.union uf.(a) uf.(b);
    
    degree.(a) <- degree.(a) + 1;
    degree.(b) <- degree.(b) + 1
  done

let () =
  for i = 1 to n do
    if degree.(i) > 2 then exit ()
  done;
  printf "Yes\n%!"
