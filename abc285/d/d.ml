open Core

let n = Scanf.scanf "%d" ident

let requests = Array.init n ~f:(fun _ -> Scanf.scanf " %s %s" Tuple2.create)

module SI = Set.Make(String)
let names =
  Array.fold requests ~init:SI.empty ~f:(fun acc (s, t) -> SI.add (SI.add acc s) t)
  |> SI.to_list
  |> List.mapi ~f:(fun i s -> s, Union_find.create i)
  |> Hashtbl.of_alist_exn ~size:n (module String)

let get = Hashtbl.find_exn names

let () =
  for i = 0 to n - 1 do
    let s, t = requests.(i) in
    if Union_find.same_class (get s) (get t) then (printf "No\n%!"; exit 0);
    Union_find.union (get s) (get t)
  done;
  printf "Yes\n%!"
