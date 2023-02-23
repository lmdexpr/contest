open Core
open Scanf

let s = scanf "%s" ident
let s = Array.init 7 ~f:(fun i ->
    match s.[i] with 'a' -> 1 | 't' -> 2 | 'c' -> 3 | 'o' -> 4 | 'd' -> 5 | 'e' -> 6 | 'r' -> 7 | _ -> 0
  )

module BIT = struct
  type 'a t = { size: int; tree: 'a array }

  let create ~size ~init = { size; tree = Array.init (size + 1) ~f:(const init) }

  let rec add bit i x =
    if i <= bit.size then begin
      bit.tree.(i) <- bit.tree.(i) + x;
      add bit (i + (i land -i)) x
    end

  let rec query ?(acc = 0) ~op bit i =
    if i <= 0 then acc
    else
      query ~op ~acc:(op acc bit.tree.(i)) bit (i - (i land -i))
end
let bit = BIT.create ~size:10 ~init:0

(* 転倒数 / inversion *)
let inversion =
  Array.foldi ~init:0 ~f:(fun i acc pos ->
      let result = BIT.query ~op:(+) bit pos in
      BIT.add bit pos 1;
      acc + i - result
    )

let () = printf "%d\n%!" @@ inversion s
