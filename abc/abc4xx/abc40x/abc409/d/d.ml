open Core
open Scanf

let t = scanf " %d" Fn.id

let solve n s =
  match
  Iter.(0 -- (n - 2))
  |> Iter.find_pred Char.(fun i -> s.[i] > s.[succ i])
  with
  | None   -> printf "%s\n%!" s
  | Some i ->
    let j =
      Iter.(succ i -- (n - 1))
      |> Iter.find_pred Char.(fun j -> s.[i] < s.[j])
      |> Option.value ~default:n
    in
    for k = 0 to i - 1 do
      printf "%c" s.[k]
    done;
    for k = i + 1 to j - 1 do
      printf "%c" s.[k]
    done;
    printf "%c" s.[i];
    for k = j to n - 1 do
      printf "%c" s.[k]
    done;
    printf "\n%!"

let () =
  for _ = 1 to t do
    let n = scanf " %d" Fn.id in
    let s = scanf " %s" Fn.id in
    solve n s;
  done
