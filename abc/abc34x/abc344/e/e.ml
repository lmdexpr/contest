open Core
open Scanf

let n = scanf "%d" Fn.id

let numbers = Hashtbl.create (module Int)
let reverse = Hashtbl.create (module Int)
let start   = ref @@ scanf " %d" Fn.id

let connect x y =
  Hashtbl.set numbers ~key:x ~data:y;
  Hashtbl.set reverse ~key:y ~data:x

let () =
  let prev = ref !start in
  for _ = 1 to n - 1 do
    let a = scanf " %d" Fn.id in
    connect !prev a;
    prev := a
  done

let q = scanf " %d" Fn.id
let () =
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 ->
      let x, y = scanf " %d %d" Tuple2.create in
      (match Hashtbl.find numbers x with
        | None   -> connect x y
        | Some z -> connect x y; connect y z;
      )
    | _ ->
      let x = scanf " %d" Fn.id in
      (match Hashtbl.find numbers x, Hashtbl.find reverse x with
      | None,   None   -> ()
      | None,   Some z -> Hashtbl.remove numbers z
      | Some y, None   -> Hashtbl.remove reverse y; start := y
      | Some y, Some z -> connect z y
      );
      Hashtbl.remove numbers x;
      Hashtbl.remove reverse x;
  done

let rec print k =
  printf "%d " k;
  match Hashtbl.find numbers k with
  | Some v -> print v
  | None   -> printf "\n"

let () = print !start
