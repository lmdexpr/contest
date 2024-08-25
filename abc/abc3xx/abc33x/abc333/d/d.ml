open Core
open Scanf

let n = scanf "%d" Fn.id

let forests = Array.init n ~f:Union_find.create
let size    = Array.create ~len:n 0

let () =
  for _ = 1 to n - 1 do
    scanf " %d %d" @@ fun u v ->
    if u <> 1 && v <> 1 then
      Union_find.union forests.(u - 1) forests.(v - 1)
  done;
  for i = 0 to n - 1 do
    let v = Union_find.get forests.(i) in
    size.(v) <- size.(v) + 1
  done

let ans = n - Option.value_exn (Array.max_elt size ~compare)

let () = printf "%d\n%!" ans
