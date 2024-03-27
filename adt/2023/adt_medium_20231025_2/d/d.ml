open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Int.pred)

let called = Array.create ~len:n false
let () =
  Array.iteri a ~f:(fun i a ->
    if not called.(i) then
      called.(a) <- true
  )

let () =
  printf "%d\n" (Array.count called ~f:not);
  Array.iteri called ~f:(fun i called ->
    if not called then
      printf "%d " (i + 1)
  );
  printf "\n"
