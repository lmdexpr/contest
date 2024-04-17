open Core
open Scanf

let n = scanf "%d" Fn.id

let uf = Array.init (n+1) ~f:Union_find.create
let () =
  for _ = 1 to n - 1 do
    scanf " %d %d" @@ fun u v ->
      if u <> 1 then
        Union_find.union uf.(u) uf.(v)
  done

let sz = Array.create ~len:(n+1) 0
let () =
  for i = 1 to n do
    let i = Union_find.get uf.(i) in
    sz.(i) <- sz.(i) + 1
  done

let ans = n - (Array.max_elt sz ~compare |> Option.value_exn)

let () = printf "%d\n%!" ans
