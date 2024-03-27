open Core
open Scanf

let n, k, d = scanf "%d %d %d" Tuple3.create

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let dp =
  Array.init (n + 1) ~f:(fun _ ->
  Array.init (n + 1) ~f:(fun _ -> 
  Array.init (d + 1) ~f:(fun _ -> -1)))

let (.!()<-) a i v = a.(i) <- max a.(i) v

let () = 
  dp.(0).(0).(0) <- 0;
  for i = 0 to n - 1 do
    for j = 0 to n do
      for k = 0 to d - 1 do
        if 0 <= dp.(i).(j).(k) then begin
          dp.(i + 1).(j    ).!(k)               <- dp.(i).(j).(k);
          dp.(i + 1).(j + 1).!((k + a.(i)) % d) <- dp.(i).(j).(k) + a.(i)
        end
      done
    done
  done

let () = printf "%d\n%!" dp.(n).(k).(0)
