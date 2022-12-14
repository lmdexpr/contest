open Core

let n, q = Scanf.scanf "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let query shifted t x y =
  let x = x - 1 and y = y -1 in
  let shifted_index i = (shifted + i) % n in
  match t with
  | 1 -> Array.swap a (shifted_index x) (shifted_index y); shifted
  | 2 -> shifted_index (n - 1)
  | _ -> printf "%d\n" a.(shifted_index x); shifted

let () =
  Iter.(1 -- q)
  |> Iter.fold (fun shifted _ -> Scanf.scanf " %d %d %d" @@ query shifted) 0
  |> ignore
