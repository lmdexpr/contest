open Core
open Scanf

let n = scanf "%d" Fn.id

let cs = Array.init n ~f:(fun i -> scanf " %d %d" @@ fun a c -> i + 1, a, c)
let () = Array.sort cs ~compare:(fun (_, _, c1) (_, _, c2) -> Int.compare c1 c2)

let ans = 
  Array.fold cs ~init:(0, []) ~f:(fun (m, deck) (i, a, _) ->
    if m < a then a, i :: deck else m, deck
  ) |> snd

let () =
  printf "%d\n" @@ List.length ans;
  List.sort ans ~compare
  |> List.iter ~f:(printf "%d "); printf "\n"
