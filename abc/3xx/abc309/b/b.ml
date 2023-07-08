open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ ->
    scanf " %s" String.to_array
    |> Array.map ~f:(fun c -> Char.to_int c - Char.to_int '0')
  )
let b = Array.init n ~f:(fun i -> Array.copy a.(i))

let () =
  b.(0).(0) <- a.(1).(0);
  for j = 1 to n - 1 do
    b.(0).(j) <- a.(0).(j - 1)
  done;
  for i = 1 to n - 2 do
    b.(i).(0)     <- a.(i + 1).(0);
    b.(i).(n - 1) <- a.(i - 1).(n - 1)
  done;
  for j = 0 to n - 2 do
    b.(n - 1).(j) <- a.(n - 1).(j + 1)
  done;
  b.(n - 1).(n - 1) <- a.(n - 2).(n - 1)

let () =
  Array.iter b ~f:(fun b ->
    Array.iter b ~f:(fun c -> printf "%d" c);
    printf "\n"
  );
