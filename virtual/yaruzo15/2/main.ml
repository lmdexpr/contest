open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id)
let t = Iter.(1 -- m) |> Iter.fold (fun acc _ -> Set.add acc @@ scanf " %s" Fn.id) String.Set.empty

let () =
  Array.iter s ~f:(fun s ->
    if Set.mem t s then printf "Yes\n" else printf "No\n"
  )
