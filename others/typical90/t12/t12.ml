open Core

let h, w = Scanf.scanf "%d %d" Tuple2.create
let q = Scanf.scanf " %d" ident

let m = Array.make_matrix ~dimx:h ~dimy:w false
let uf = Array.init h ~f:(fun i -> Array.init w ~f:(fun j -> Union_find.create (i, j)))

let to_idx a b = a - 1, b - 1

let query1 () =
  let r, c = Scanf.scanf " %d %d" to_idx in
  m.(r).(c) <- true;
  [ -1, 0; 1, 0; 0, -1; 0, 1 ]
  |> List.iter ~f:(fun (dx, dy) ->
      let rd, cd = r + dx, c + dy in
      if 0 <= rd && rd < h && 0 <= cd && cd < w && m.(rd).(cd) then
        Union_find.union uf.(r).(c) uf.(rd).(cd)
    )

let query2 () =
  let ra, ca = Scanf.scanf " %d %d" to_idx
  and rb, cb = Scanf.scanf " %d %d" to_idx in
  let p = m.(ra).(ca) && m.(rb).(cb) && Union_find.same_class uf.(ra).(ca) uf.(rb).(cb) in
  print_endline @@ if p then "Yes" else "No"

let query n = if n = 1 then query1 () else query2 ()

let () =
  for _ = 1 to q do
    query @@ Scanf.scanf " %d" ident
  done
