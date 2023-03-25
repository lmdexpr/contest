open Core
open Scanf

let r, c = scanf "%d %d" Tuple2.create
let b = Array.init r ~f:(fun _ -> scanf " %s" String.to_array)

let () =
  for i = 0 to r - 1 do
    for j = 0 to c - 1 do
      match b.(i).(j) with
      | '.' | '#' -> ()
      | n ->
        let n = Char.to_int n - Char.to_int '0' in
        for i2 = 0 to r - 1 do
          for j2 = 0 to c - 1 do
            if abs (i - i2) + abs (j - j2) <= n && Char.(b.(i2).(j2) = '#') then
              b.(i2).(j2) <- '.'
          done
        done;
        b.(i).(j) <- '.'
    done
  done


let () = Array.iter b ~f:(fun b -> Array.iter b ~f:(printf "%c"); printf "\n%!")
