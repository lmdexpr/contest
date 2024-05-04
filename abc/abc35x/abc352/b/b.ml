open Core
open Scanf

let s, t = scanf "%s %s" @@ fun s t -> String.to_list s, t

let rec solve i = function
  | _ when i = String.length t -> ()
  | [] -> ()
  | c :: _ as s when Char.(c <> t.[i]) -> solve (i + 1) s
  | _ :: rest ->
    let i = i + 1 in
    printf "%d " i;
    solve i rest

let () = solve 0 s; printf "\n%!"
