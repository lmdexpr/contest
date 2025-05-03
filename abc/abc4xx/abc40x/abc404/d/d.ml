open Core
open Scanf

let rec cartesian_power iter n =
  if n <= 0 then Iter.singleton []
  else
    Iter.flat_map
      (fun rest ->
        iter |> Iter.map (fun d -> d :: rest )
      )
      (cartesian_power iter @@ n - 1)

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

let c = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let a = Array.init m ~f:(fun _ ->
  let k = scanf " %d" Fn.id in
  Array.init k ~f:(fun _ -> scanf " %d" Int.pred)
)

let animals = Array.init n ~f:(fun _ -> Int.Set.empty)
let () =
  for i = 0 to m - 1 do
    Array.iter a.(i) ~f:(fun j ->
      animals.(j) <- Set.add animals.(j) i
    )
  done

let ans =
  cartesian_power Iter.(0 -- 2) n
  |> Iter.map (fun cs ->
    let satisfied = Array.init m ~f:(const 0) in
    List.iteri cs ~f:(fun i c ->
      animals.(i)
      |> Set.iter ~f:(fun animal ->
        satisfied.(animal) <- satisfied.(animal) + c
      )
    );
    if Array.for_all satisfied ~f:(fun s -> s >= 2) then
      List.foldi cs ~init:0 ~f:(fun i acc count -> acc + count * c.(i))
    else
      Int.max_value
  )
  |> Iter.min_exn ~lt:(<)

let () = printf "%d\n%!" ans
