open Core
open Scanf

let n = scanf "%d" Fn.id
let posts = Array.init n ~f:(fun i -> i + 1, scanf " %s %d" Tuple2.create)

let _ =
  Array.foldi posts ~init:String.Set.empty ~f:(fun i set (j, (s, _)) ->
    if Set.mem set s then
      posts.(i) <- j, (s, 0);
    Set.add set s
  )

let ans =
  Array.max_elt posts ~compare:(fun (_, (_, a)) (_, (_, b)) -> Int.compare a b)
  |> Option.value_exn |> fst

let () = printf "%d\n%!" ans
