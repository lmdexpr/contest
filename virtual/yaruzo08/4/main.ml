open Core
open Scanf

let _, _, n = scanf "%d %d %d" Tuple3.create

let a = Array.init n ~f:(const 0)
let b = Array.init n ~f:(const 0)
let () =
  for i = 0 to n - 1 do
    a.(i) <- scanf " %d" ident;
    b.(i) <- scanf " %d" ident
  done

let enum_x = Hashtbl.create (module Int)
let enum_y = Hashtbl.create (module Int)

let solve arr enum =
  Int.Set.of_array arr
  |> Int.Set.to_array
  |> Array.iteri ~f:(fun i x -> Hashtbl.add_exn enum ~key:x ~data:(i + 1))

let () =
  solve a enum_x;
  solve b enum_y;
  for i = 0 to n - 1 do
    printf "%d %d\n" (Hashtbl.find_exn enum_x a.(i)) (Hashtbl.find_exn enum_y b.(i))
  done
