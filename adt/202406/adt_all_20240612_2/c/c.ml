open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ -> scanf " %d" Fn.id))
let b = Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ -> scanf " %d" Fn.id))

let rot a =
  let a' = Array.copy_matrix a in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      a'.(i).(j) <- a.(j).(n - i - 1)
    done
  done;
  a'

let compara a b =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if a.(i).(j) = 1 && b.(i).(j) = 0 then 
        raise Exit
    done
  done

let () =
  let a = ref a in
  for _ = 1 to 4 do
    a := rot !a;
    try
      compara !a b;
      printf "Yes\n";
      exit 0
    with Exit -> ();
  done;
  printf "No\n"
