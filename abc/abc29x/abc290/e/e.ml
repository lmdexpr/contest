open Core
open Scanf

let n = scanf "%d" ident

let p = Array.init (n+1) ~f:(const [])
let () =
  for i = 1 to n do
    let a = scanf " %d" ident in
    p.(a) <- Int64.of_int i :: p.(a)
  done

let all =
  Iter.(1 -- n)
  |> Iter.map Int64.(fun i -> (of_int n + 1L - of_int i) * (of_int i / 2L))
  |> Iter.fold Int64.(+) 0L

let rec solve ?(acc=0L) p l r =
  if l >= r then acc
  else
    let r_l = Int64.of_int (r - l) in
    let pl = p.(l) and pr = Int64.(of_int n + 1L - p.(r)) in
    if Int64.(pl < pr) then
      solve p ~acc:Int64.(acc + r_l * pl) (l + 1) r
    else 
      solve p ~acc:Int64.(acc + r_l * pr) l (r - 1)

let same =
  Iter.of_array p
  |> Iter.map (fun p -> solve (Array.of_list_rev p) 0 (List.length p - 1))
  |> Iter.fold Int64.(+) 0L

let ans = Int64.(all - same)

let () = printf "%Ld\n%!" ans
