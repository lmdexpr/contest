open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create
let p =
  List.init n ~f:(fun i -> n - i, scanf " %d %d %d" @@ fun a b c -> a + b + c)
  |> List.sort ~compare:(fun (_, a) (_, b) -> Int.descending a b)

let ok, rest = List.split_n p (k - 1)

let ok = Int.Set.of_list @@ List.map ok ~f:fst

let ok =
  match rest with 
  | []       -> ok
  | [ i, _ ] -> Set.add ok i
  | (i, p) :: rest ->
    let ok = Set.add ok i in
    List.take_while rest ~f:(fun (_, q) -> p <= q + 300)
    |> List.fold ~init:ok ~f:(fun ok (i, _) -> Set.add ok i)

let () = 
  for i = 1 to n do
    printf @@ if Set.mem ok i then "Yes\n" else "No\n"
  done
