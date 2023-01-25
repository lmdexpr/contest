open Core

let n = Scanf.scanf "%d" ident

let scan _ = Scanf.scanf " %d" ident

let a = Array.init n ~f:scan

let q = scan ()

module SI = Set.Make(Int)

let query (base, added) =
  match scan () with
  | 1 ->
    let x = scan () in
    List.iter added ~f:(fun i -> a.(i) <- 0);
    x, []
  | 2 ->
    let i, x = Scanf.scanf " %d %d" @@ fun i x -> i - 1, x in
    a.(i) <- a.(i) + x;
    base, i :: added
  | _ ->
    let i = scan () in
    printf "%d\n" @@ base + a.(i - 1);
    base, added

let rec do_ i ~times ~f ~init =
  if i <> times then
    let init = f init in
    do_ (i + 1) ~times ~f ~init
let do_ = do_ 0

let () = do_ ~times:q ~f:query ~init:(0, List.init n ~f:ident)
