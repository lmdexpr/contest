open Core
open Scanf

let n = scanf " %d" Fn.id
let l = scanf " %d" Fn.id

let d = Array.init (n-1) ~f:(fun _ -> scanf " %d" Fn.id)

let () =
  if l % 3 <> 0 then
    (printf "0\n%!"; exit 0)

let p = Array.create ~len:l Int.Set.empty
let () =
  p.(0) <- Int.Set.singleton 0;
  Array.foldi d ~init:0 ~f:(fun i len x ->
    let len = (len + x) % l in
    p.(len) <- Set.add p.(len) (i + 1);
    len
  )
  |> ignore

let points i = Set.length p.(i)

let ans =
  Iter.(0 -- (l / 3 - 1))
  |> Iter.fold
    (fun ans i ->
      ans + points i * points ((i + l / 3) % l) * points ((i + 2 * l / 3) % l)
    )
    0

let () = printf "%d\n%!" ans
