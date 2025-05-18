open Core
open Scanf

let n = scanf " %d" Fn.id
let p = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

type t =
  | Asc  of int
  | Desc of int

let ans =
  let find_local f =
    let rec aux i =
      if n - 2 < i then n - 1
      else if not @@ f p.(i) p.(i + 1) then i 
      else 
        aux (i + 1)
    in
    aux
  in
  let rec compress start acc =
    if n - 1 <= start then acc
    else (
      let k = find_local (<) start in
      let m = find_local (>) k in
      compress m 
        Iter.(append acc @@ doubleton (Asc (k - start)) (Desc (m - k)))
    )
  in
  let compressed = compress 0 Iter.empty |> Iter.to_array in
  let n = Array.length compressed in
  Iter.(1 -- (n - 2))
  |> Fn.flip Iter.fold 0
    (fun acc i ->
      match compressed.(i-1), compressed.(i), compressed.(i + 1) with
      | Asc x, Desc _, Asc y -> acc + x * y
      | _                    -> acc
    )

let () = printf "%d\n%!" ans
