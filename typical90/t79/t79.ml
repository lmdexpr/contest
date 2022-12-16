open Core

let h, w = Scanf.scanf "%d %d" Tuple2.create

let scan_int _ = Scanf.scanf " %d" ident
let a = Array.init h ~f:(fun _ -> Array.init w ~f:scan_int)
let b = Array.init h ~f:(fun _ -> Array.init w ~f:scan_int)

let count =
  Iter.(0 -- (h - 2)) |> Iter.flat_map (fun i ->
      Iter.(0 -- (w - 2)) |> Iter.map (fun j ->
          let count = b.(i).(j) - a.(i).(j) in
          a.(i  ).(j)   <- a.(i  ).(j)   + count;
          a.(i+1).(j)   <- a.(i+1).(j)   + count;
          a.(i  ).(j+1) <- a.(i  ).(j+1) + count;
          a.(i+1).(j+1) <- a.(i+1).(j+1) + count;
          abs count
        )
    )
  |> Iter.sum

let yes =
  Iter.(0 -- (h - 1)) |> Iter.fold (fun acc i -> acc && a.(i).(w-1) = b.(i).(w-1)) true
  &&
  Iter.(0 -- (w - 1)) |> Iter.fold (fun acc j -> acc && a.(h-1).(j) = b.(h-1).(j)) true

let () =
  if yes then printf "Yes\n%d\n%!" count
  else
    print_endline "No"
