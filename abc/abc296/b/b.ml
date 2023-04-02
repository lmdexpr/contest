open Core
open Scanf

let s = Array.init 8 ~f:(const "")
let () =
  scanf "%s" @@ Array.set s 0;
  for i = 1 to 7 do
    scanf " %s" @@ Array.set s i
  done

let () =
  for i = 0 to 7 do
    for j = 0 to 7 do
      if Char.(s.(i).[j] = '*') then
        printf "%c%d\n%!" (Char.of_int_exn (j + 97)) (8 - i)
    done
  done
