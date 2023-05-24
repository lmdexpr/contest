open Core
open Scanf

let yes tf = if tf then "Yes" else "No"
let yes tf = printf "%s\n%!" @@ yes tf

let solve n k s =
  let s = String.to_array s in
  let a = Array.init (n+1) ~f:(const 0) in
  let b = Array.init (n+1) ~f:(const 0) in
  let c = Array.init (n+1) ~f:(const 0) in
  Array.iteri s ~f:(fun i ->
      a.(i + 1) <- a.(i); b.(i + 1) <- b.(i); c.(i + 1) <- c.(i);
      function
      | '0' -> a.(i + 1) <- a.(i) + 1
      | '1' -> b.(i + 1) <- b.(i) + 1
      | _   -> c.(i + 1) <- c.(i) + 1
    );
  Iter.(k -- n)
  |> Iter.filter (fun r -> a.(r) - a.(r - k) = 0 && b.(r) - b.(r - k) = b.(n))
  |> Iter.length
  |> (=) 1
  |> yes

let t = scanf "%d" ident
let () =
  for _ = 1 to t do
    scanf " %d %d %s" solve
  done
