open Core
open Scanf

let n, t = scanf "%d %s" Tuple2.create
let s = Array.init n ~f:(fun _ -> scanf " %s" Fn.id)

let t_len = String.length t

let ans =
  Iter.(1 -- n)
  |> Iter.filter (fun i ->
    let i = i - 1 in
    let s = s.(i) in let s_len = String.length s in
    let n = Int.min s_len t_len in
    let l =
      Iter.(0 -- (n - 1))
      |> Iter.find_pred Char.(fun j -> s.[j] <> t.[j]) 
      |> Option.value ~default:n
    in
    let r =
      Iter.(0 -- (n - 1))
      |> Iter.find_pred Char.(fun j -> s.[s_len - 1 - j] <> t.[t_len - 1 - j]) 
      |> Option.value ~default:n
    in
    not @@
    (s_len = t_len && l = r && r = s_len) ||
    (s_len = t_len - 1 && l + r >= s_len) || 
    (s_len = t_len + 1 && l + r >= s_len) ||
    (s_len = t_len && l + r + 1 = t_len)
  )

let () =
  printf "%d\n%!" @@ Iter.length ans;
  Iter.iter (printf "%d ") ans;
  printf "\n%!"
