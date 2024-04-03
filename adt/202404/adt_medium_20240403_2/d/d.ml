open Core
open Scanf

let s = Array.create ~len:10 [||]
let () =
  s.(0) <- scanf "%s" String.to_array;
  for i = 1 to 9 do
    s.(i) <- scanf " %s" String.to_array
  done

let a = ref Int.max_value
let b = ref Int.min_value
let c = ref Int.max_value
let d = ref Int.min_value

let () =
  for i = 1 to 10 do
    for j = 1 to 10 do
      if Char.equal s.(i - 1).(j - 1) '#' then begin
        a := min !a i;
        b := max !b i;
        c := min !c j;
        d := max !d j
      end
    done
  done

let () =
  printf "%d %d\n" !a !b;
  printf "%d %d\n" !c !d
