open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun i ->
  let c = scanf " %d" Fn.id in
  i + 1, Array.init c ~f:(fun _ -> scanf " %d" Fn.id)
)
let x = scanf " %d" Fn.id

let a =
  a
  |> Array.filter ~f:(fun (_, a) -> Array.mem ~equal a x)
  |> Array.map    ~f:(fun (i, a) -> i, Array.length a)

let min =
  Array.min_elt a ~compare:(fun (_, l) (_, r) -> Int.compare l r)
  |> Option.value_map ~f:snd ~default:0

let ans =
  Array.filter a ~f:(fun (_, a) -> a = min)

let () =
  printf "%d\n%!" @@ Array.length ans;
  Array.iter ans ~f:(fun (i, _) -> printf "%d " i); printf "\n%!"
