open Core
open Scanf

let n = scanf "%d" Fn.id

let s = Array.init n ~f:(fun _ -> scanf " %s" String.to_array |> Array.map ~f:Char.((=) 'o'))

let ans =
  Iter.(0 -- (n - 1))
  |> Iter.map (fun i -> Iter.(0 -- (n - 1)) |> Iter.filter_count (fun j -> s.(i).(j)), i + 1)
  |> Iter.sort ~cmp:(Tuple2.compare ~cmp1:Int.descending ~cmp2:Int.compare)

let () =
  Iter.iter (fun (_, i) -> printf "%d " i) ans; printf "\n"
