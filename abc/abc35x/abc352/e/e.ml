open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let a =
  Array.init m ~f:(fun _ ->
    let k, c = scanf " %d %d" Tuple2.create in
    c, Array.init k ~f:(fun _ -> scanf " %d" Fn.id)
  )

let () = Array.sort a ~compare:(fun (c1, _) (c2, _) -> compare c1 c2)

let uf = Array.init (n + 1) ~f:Union_find.create

let cost =
  Iter.of_array a
  |> Iter.fold (fun acc (c, a) ->
    let forest = 
      Array.map a ~f:(fun a -> Union_find.get uf.(a))
      |> Int.Set.of_array
    in
    let len = Set.length forest in
    let a = Set.to_array forest in
    for i = 0 to len - 2 do
      Union_find.union uf.(a.(i)) uf.(a.(i + 1))
    done;
    (len - 1) * c + acc
  ) 0

let comps =
  Iter.(1 -- n) |> Iter.map (fun i -> Union_find.get uf.(i))
  |> Iter.to_array |> Int.Set.of_array |> Set.length
  
let ans = if comps = 1 then cost else -1

let () = printf "%d\n%!" ans
