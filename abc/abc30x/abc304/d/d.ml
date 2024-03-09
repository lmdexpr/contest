open Core
open Scanf

let w, h = scanf "%d %d" Tuple2.create

let n = scanf " %d" ident
let p = Array.create ~len:n 0 and q = Array.create ~len:n 0
let () =
  for i = 0 to n - 1 do
    let pi, qi = scanf " %d %d" Tuple2.create in
    p.(i) <- pi; q.(i) <- qi
  done

let a =
  let len = scanf " %d" ident in
  let a = Array.create ~len:(len+2) 0 in
  for i = 1 to len do
    a.(i) <- scanf " %d" ident
  done;
  a.(len + 1) <- w; a

let b =
  let len = scanf " %d" ident in
  let b = Array.create ~len:(len+2) 0 in
  for i = 1 to len do
    b.(i) <- scanf " %d" ident
  done;
  b.(len + 1) <- h; b

let mp = Hashtbl.create
    (module struct
      type t = int * int
      let hash = Hashtbl.hash
      let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
      let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    end)

let lower_bound a x =
  Array.binary_search a ~compare `First_strictly_greater_than x
  |> Option.value ~default:(Array.length a - 1)

let () =
  for i = 0 to n - 1 do
    Hashtbl.incr mp (lower_bound a p.(i), lower_bound b q.(i))
  done

let max = Hashtbl.fold mp ~init:0 ~f:(fun ~key:_ ~data acc -> max acc data)
let min =
  if Hashtbl.length mp <> Array.((length a - 1) * (length b - 1)) then 0
  else
    Hashtbl.fold mp ~init:n ~f:(fun ~key:_ ~data acc -> min acc data)

let () = printf "%d %d\n%!" min max
