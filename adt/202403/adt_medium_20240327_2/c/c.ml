open Core
open Scanf

let n = scanf "%d" Fn.id
let x, y, z = scanf " %d %d %d" Tuple3.create

let a = Array.init n ~f:(fun i -> i + 1, scanf " %d" Fn.id)
let b = Array.init n ~f:(fun i -> i + 1, scanf " %d" Fn.id)
let c =  List.init n ~f:(fun i -> i + 1, snd a.(i) + snd b.(i))

let a = Array.to_list a
let b = Array.to_list b

let ans =
  let compare (_, x) (_, y) = Int.descending x y in
  let passed =
    List.sort a ~compare 
    |> fun a -> List.take a x 
    |> List.map ~f:fst |> Int.Set.of_list
  in
  let passed =
    List.sort b ~compare
    |> List.filter ~f:(fun (i, _) -> not (Set.mem passed i))
    |> fun b -> List.take b y
    |> List.map ~f:fst |> Int.Set.of_list
    |> Set.union passed
  in
  let passed =
    List.sort c ~compare
    |> List.filter ~f:(fun (i, _) -> not (Set.mem passed i))
    |> fun c -> List.take c z
    |> List.map ~f:fst |> Int.Set.of_list
    |> Set.union passed
  in
  passed

let () = Set.iter ans ~f:(printf "%d\n")
