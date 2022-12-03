open Core

let n = Scanf.scanf "%d" ident
let s = List.init n ~f:(fun _ -> Scanf.scanf " %d" ident) |> List.rev

let rec solve ?(acc=0) = function
  | [] -> ()
  | x :: xs ->
    let a = x - acc in
    printf "%d " a;
    let acc = acc + a in
    solve ~acc xs

let () =
  solve s;
  print_endline "";
