open Core
open Scanf

let n = scanf "%d" Fn.id
let x, y, z = scanf " %d %d %d" Tuple3.create

let a = Array.init n ~f:(fun i -> scanf " %d" Fn.id, i+1)
let b = Array.init n ~f:(fun i -> scanf " %d" Fn.id, i+1)
let c = Array.init n ~f:(fun i -> fst a.(i) + fst b.(i), i+1)

let compare = Tuple2.compare ~cmp1:Int.descending ~cmp2:Int.ascending

let ans =
  Array.to_list a
  |> List.sort ~compare
  |> Fn.flip List.take x
  |> List.map ~f:snd |> Int.Set.of_list

let ans = 
  Array.to_list b 
  |> List.filter ~f:(fun (_, i) -> not @@ Set.mem ans i) 
  |> List.sort ~compare
  |> Fn.flip List.take y
  |> List.map ~f:snd |> Int.Set.of_list |> Set.union ans

let ans =
  Array.to_list c
  |> List.filter ~f:(fun (_, i) -> not @@ Set.mem ans i)
  |> List.sort ~compare
  |> Fn.flip List.take z 
  |> List.map ~f:snd |> Int.Set.of_list |> Set.union ans

let () = Set.iter ans ~f:(printf "%d\n")
