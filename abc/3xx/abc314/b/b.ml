open Core
open Scanf

let n = scanf "%d" Fn.id

let mem_of_num = Array.init 38 ~f:(const [])
let num_of_mem = Array.init n ~f:(fun i ->
  let c = scanf " %d" Fn.id in
  Array.init c ~f:(fun _ -> scanf " %d" Fn.id)
  |> Array.iter ~f:(fun a -> mem_of_num.(a) <- i :: mem_of_num.(a));
  c
)

let x = scanf " %d" Fn.id

let ans =
  mem_of_num.(x)
  |> List.map ~f:(fun i -> num_of_mem.(i), i + 1)
  |> List.sort ~compare:(Tuple2.compare ~cmp1:compare ~cmp2:compare)

let ans =
  match List.hd ans with
  | None -> []
  | Some (hd, _) ->
    List.filter ans ~f:(fun (c, _) -> c = hd)
    |> List.map ~f:(fun (_, i) -> i)

let () =
  printf "%d\n%!" (List.length ans);
  List.iter ans ~f:(printf "%d ");
  printf "\n%!"
