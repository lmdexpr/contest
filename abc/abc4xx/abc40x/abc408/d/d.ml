open Core
open Scanf

let solve n s =
  let zs = Array.create ~len:(n + 1) 0 in
  let os = Array.create ~len:(n + 1) 0 in
  for i = 0 to n - 1 do
    os.(i + 1) <- os.(i) + Bool.to_int s.(i);
    zs.(i + 1) <- zs.(i) + Bool.to_int (not s.(i));
  done;
  let score = Array.init (n + 1) ~f:(fun i -> zs.(i) - os.(i)) in
  let ans, _ =
    Iter.(0 -- n)
    |> Iter.fold (fun (ans, acc) i ->
      min ans (score.(i) - acc),
      max acc score.(i)
    ) (0, 0)
  in
  os.(n) + ans

let t = scanf " %d" Fn.id

let () =
  for _ = 1 to t do
    let n = scanf " %d" Fn.id in
    scanf " %s" String.to_array
    |> Array.map ~f:Char.((=) '1')
    |> solve n
    |> printf "%d\n%!"
  done
