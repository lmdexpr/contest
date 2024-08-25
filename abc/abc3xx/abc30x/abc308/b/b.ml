open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let c = Array.init n ~f:(fun _ -> scanf " %s" ident)
let d = Array.init m ~f:(fun _ -> scanf " %s" ident)
let p0 = scanf " %d" ident
let p = Array.init m ~f:(fun _ -> scanf " %d" ident)

let table = Hashtbl.create ~size:m (module String)
let () =
  for i = 0 to m - 1 do
    let d = d.(i) and p = p.(i) in
    Hashtbl.set table ~key:d ~data:p
  done

let ans = Array.fold c ~init:0 ~f:(fun acc s ->
    acc + (Hashtbl.find table s |> Option.value ~default:p0)
  )

let () = printf "%d\n%!" ans
