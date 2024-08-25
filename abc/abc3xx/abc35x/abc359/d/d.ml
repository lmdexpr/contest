open Core
open Scanf

let _n, k = scanf "%d %d" Tuple2.create
let s = scanf " %s" Fn.id

let modulo = 998244353
let (+%) a b = (a % modulo + b % modulo) % modulo

let ans =
  let init = 
    Hashtbl.of_alist_exn (module String) [ String.make (k - 1) 'X', 1 ] 
  in
  String.fold s ~init ~f:(fun dp c ->
    let nx = Hashtbl.create (module String) in
    Hashtbl.fold dp ~init:[] ~f:(fun ~key ~data acc ->
      let add a b acc = 
        if Char.(c = a) then acc else (key ^ b, data) :: acc 
      in
      acc |> add 'B' "A" |> add 'A' "B"
    )
    |> Hashtbl.of_alist_exn (module String)
    |> Hashtbl.iteri ~f:(fun ~key ~data ->
      if String.(key <> rev key) then
        Hashtbl.update nx (String.drop_prefix key 1) 
          ~f:(Option.value_map ~default:data ~f:((+%) data))
    );
    nx
  )
  |> Hashtbl.fold ~init:0 ~f:(fun ~key:_ ~data -> (+%) data)

let () = printf "%d\n%!" ans
