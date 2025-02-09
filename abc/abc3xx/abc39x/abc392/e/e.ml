open Core
open Scanf

let n, m = scanf " %d %d" Tuple2.create

let es = Array.init m ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let uf = Array.init (n + 1) ~f:Union_find.create
let es =
  Array.foldi es ~init:[] ~f:(fun i acc (a, b) ->
    let acc =
      if Union_find.same_class uf.(a) uf.(b) then
        (i + 1, a, b) :: acc
      else
        acc
    in
    Union_find.union uf.(a) uf.(b);
    acc
  )

let connected =
  Iter.(1 -- n)
  |> Iter.map (fun i -> Union_find.get uf.(i))
  |> Iter.to_array
  |> Int.Set.of_array

let () = 
  printf "%d\n%!" (Set.length connected - 1)

let () =
  List.fold es ~init:connected ~f:(fun acc (i, a, b) ->
    let acc = Set.remove acc Union_find.(get uf.(a)) in
    match Set.nth acc 0 with
    | None   -> acc
    | Some x ->
      printf "%d %d %d\n" i b x;
      Union_find.union uf.(a) uf.(x);
      acc
      |> Fn.flip Set.remove x
      |> Fn.flip Set.add Union_find.(get uf.(x))
  )
  |> ignore
