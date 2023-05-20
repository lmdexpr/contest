open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let value = Array.make_matrix ~dimx:n ~dimy:n 0
let dist  = Array.make_matrix ~dimx:n ~dimy:n n
let () =
  for i = 0 to n - 1 do
    dist.(i).(i) <- 0;
    let s = scanf " %s" ident in
    for j = 0 to n - 1 do
      if Char.(s.[j] = 'Y') then (
        dist.(i).(j)  <- 1;
        value.(i).(j) <- a.(j)
      )
    done
  done

let _warshall_floyd =
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        let detour = dist.(i).(k) + dist.(k).(j) in
        if detour = dist.(i).(j) then
          value.(i).(j) <- max value.(i).(j) @@ value.(i).(k) + value.(k).(j)
        else if detour < dist.(i).(j) then (
          dist.(i).(j)  <- detour;
          value.(i).(j) <- value.(i).(k) + value.(k).(j)
        )
      done
    done
  done

let q = scanf " %d" ident
let () =
  for _ = 1 to q do
    let u, v = scanf " %d %d" @@ fun u v -> u - 1, v - 1 in
    if dist.(u).(v) = n then
      printf "Impossible\n%!"
    else
      printf "%d %d\n%!" dist.(u).(v) (value.(u).(v) + a.(u))
  done
