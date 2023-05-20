open Core
open Scanf

let n = scanf "%d" ident

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let ameba = Array.init (2 * n + 2) ~f:(const 0)

let () =
  Array.iteri a
    ~f:(fun i a ->
        let i = i + 1 in
        ameba.(2 * i)     <- ameba.(a) + 1;
        ameba.(2 * i + 1) <- ameba.(a) + 1
      )

let () =
  for i = 1 to 2 * n + 1 do
    printf "%d\n%!" ameba.(i);
  done
