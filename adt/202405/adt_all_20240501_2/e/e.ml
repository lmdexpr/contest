open Core
open Scanf

let n = scanf "%d" Fn.id

let ab = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)
let uf = Hashtbl.create (module Int)

let f i = function
  | None    -> Union_find.create i
  | Some uf -> uf

let () =
  Hashtbl.set uf ~key:1 ~data:(Union_find.create 1)

let set =
  Array.fold ab ~init:Int.Set.empty ~f:(fun acc (a, b) ->
    Hashtbl.update uf a ~f:(f a);
    Hashtbl.update uf b ~f:(f b);
    Union_find.union (Hashtbl.find_exn uf a) (Hashtbl.find_exn uf b);
    Set.add (Set.add acc a) b
  )

let ans =
  Set.to_list set
  |> Iter.of_list
  |> Iter.rev
  |> Iter.find_pred (fun i -> Union_find.same_class (Hashtbl.find_exn uf 1) (Hashtbl.find_exn uf i))
  |> Option.value ~default:1

let () = printf "%d\n%!" ans
