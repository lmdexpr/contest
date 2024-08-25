open Core

let n = Scanf.scanf "%d" ident

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let scan_next () = Scanf.scanf " %d" ident

let q = scan_next ()

let () =
  for _ = 1 to q do
    let query = scan_next () in
    let k = scan_next () - 1 in
    match query with
    | 2 -> printf "%d\n%!" a.(k)
    | _ -> a.(k) <- scan_next ()
  done
