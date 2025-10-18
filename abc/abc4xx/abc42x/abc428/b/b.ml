open Core
open Scanf

let n = scanf " %d" Fn.id
let k = scanf " %d" Fn.id

let s = scanf " %s" Fn.id

let tbl = Hashtbl.create (module String)
let () =
  for pos = 0 to n - k do
    Hashtbl.update tbl (String.sub s ~pos ~len:k) ~f:(function
      | None   -> 1
      | Some x -> x + 1
    )
  done

let x =
  Hashtbl.fold tbl ~init:0 ~f:(fun ~key:_ ~data acc -> max data acc)

let ans =
  Hashtbl.fold tbl ~init:String.Set.empty ~f:(fun ~key ~data acc ->
    if data <> x then acc
    else
      Set.add acc key
  )

let () =
  printf "%d\n" x;
  Set.iter ans ~f:(printf "%s ");
