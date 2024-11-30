open Core
open Scanf

let n = scanf "%d" Fn.id

let x, y, z = scanf " %d %d %d" Tuple3.create

let a = List.init n ~f:(fun i -> scanf " %d" @@ fun x -> x, n - i)
let b = List.init n ~f:(fun i -> scanf " %d" @@ fun x -> x, n - i)
let c = List.zip_exn a b |> List.map ~f:(fun ((x, i), (y, _)) -> x + y, i)

let compare = Tuple2.compare ~cmp1:Int.descending ~cmp2:Int.ascending
let a, b, c =
  List.sort a ~compare, List.sort b ~compare, List.sort c ~compare

let ans = List.take a x |> List.map ~f:snd |> Int.Set.of_list

let b   = List.filter ~f:(fun (_, i) -> not @@ Set.mem ans i) b
let ans = List.take b y |> List.map ~f:snd |> Int.Set.of_list |> Set.union ans

let c   = List.filter ~f:(fun (_, i) -> not @@ Set.mem ans i) c
let ans = List.take c z |> List.map ~f:snd |> Int.Set.of_list |> Set.union ans

let () = 
  Set.iter ans ~f:(fun x -> printf "%d\n" x)
