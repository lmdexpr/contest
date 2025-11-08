open Core
open Scanf

let x = scanf " %d" Fn.id
let n = scanf " %d" Fn.id

let w = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let attached = Array.create ~len:n false
let weight = ref x

let q = scanf " %d" Fn.id
let () =
  for _ = 1 to q do
    let p = scanf " %d" Fn.id in

    weight := !weight + (if attached.(p-1) then -1 else 1) * w.(p-1);
    attached.(p-1) <- not attached.(p-1);

    printf "%d\n%!" !weight;
  done
