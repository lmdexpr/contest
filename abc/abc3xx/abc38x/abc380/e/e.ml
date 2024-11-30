open Core
open Scanf

let n, q = scanf "%d %d" Tuple2.create

let uf = Array.init n ~f:(fun i -> Union_find.create i)
let ufget x   = Union_find.get uf.(x)
let union x y = Union_find.union uf.(x) uf.(y)

let square = Array.init n ~f:(fun i -> i - 1, i, i + 1)
let color  = Array.init (n + 1) ~f:(const 1)

let () = 
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 ->
      let x, c = scanf " %d %d" @@ fun x c -> x - 1, c in
      let l, r, d = square.(ufget x) in
      color.(d) <- color.(d) - (r - l);
      color.(c) <- color.(c) + (r - l);
      let l =
        if l < 0 then l
        else
          let pl, _, pc = square.(ufget l) in
          if pc <> c then l
          else 
            (union l x; pl)
      in
      let r =
        if n - 1 <= r then r
        else
          let _, nr, nc = square.(ufget (r + 1)) in
          if nc <> c then r
          else
            (union x (r + 1); nr)
      in
      square.(ufget x) <- l, r, c;
    | _ ->
      let c = scanf " %d" Fn.id in
      printf "%d\n%!" color.(c)
  done
