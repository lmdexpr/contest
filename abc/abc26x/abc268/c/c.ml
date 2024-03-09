open Core
open Scanf

let n = scanf "%d" ident

let p = Array.init n ~f:(fun _ -> scanf " %d" ident)

let count = Array.init n ~f:(const 0)
let () =
  Iter.of_array p |> Iter.iteri (fun i p ->
      Iter.(0 -- 2) |> Iter.iter (fun j ->
          let idx = (p - 1 - i + j + n) % n in
          count.(idx) <- count.(idx) + 1
        )
    )

let ans = Array.max_elt count ~compare |> Option.value ~default:0

let () = printf "%d\n%!" ans
