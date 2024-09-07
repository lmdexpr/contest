open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun i ->
    Array.init (i + 1) ~f:(fun _ -> scanf " %d" @@ fun x -> x - 1)
  )

let ans =
  Iter.(0 -- (n - 1))
  |> Fn.flip Iter.fold 0 (fun acc i ->
    let i = max acc i and j = min acc i in
    a.(i).(j)
  )

let () = printf "%d\n%!" @@ ans + 1
