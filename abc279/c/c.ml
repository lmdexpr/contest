open Core

let h, w = Scanf.scanf "%d %d" Tuple2.create

let s = Array.init h ~f:(fun _ -> Scanf.scanf " %s" ident) 
let s = Array.init w ~f:(fun i -> String.init h ~f:(fun j -> s.(j).[i]))

let t = Array.init h ~f:(fun _ -> Scanf.scanf " %s" ident)
let t = Array.init w ~f:(fun i -> String.init h ~f:(fun j -> t.(j).[i]))

let rec find_index arr ?(pos=0) ?(length=Array.length arr) ~f =
  if pos >= length then None
  else if f pos arr.(pos) then Some pos
  else
    find_index arr ~length ~f ~pos:(pos+1) 

let satisfied =
  Array.fold_until t ~init:0 ~finish:(const true) ~f:(fun i t ->
      let open Continue_or_stop in
      match find_index s ~pos:i ~length:w ~f:(fun _ s -> String.(s = t)) with
      | None   -> Stop false
      | Some j ->
        Array.swap s i j;
        Continue (i+1)
    )

let () = print_endline @@ if satisfied then "Yes" else "No"
