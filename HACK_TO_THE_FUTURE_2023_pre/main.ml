open Core
open Printf

let m, _eps = Scanf.scanf "%d %f" @@ fun m e -> m, e

let n = m

let () = 
  printf "%d\n" n;
  let n = n - 1 in
  for k = 0 to m - 1 do
    let f i = 
      let head = if i < k then "1" else "0"  in
      head ^ String.make (n - 1 - i) '0'
    in
    let g = List.init n ~f in
    print_endline @@ String.concat g;
    Out_channel.(flush stdout)
  done

let unconnected s =
  let connceted = Array.create ~len:n false in
  let rec unconnected count i s =
    let len = List.length s in
    if i = n || len = 0 then count
    else
      let s, rest = List.split_n s @@ n - 1 - i in 

      let con = List.filter_mapi s ~f:(fun j e -> Option.some_if Char.(e = '1') @@ i + 1 + j) in
      List.iter con ~f:(fun e -> connceted.(e) <- true);

      let count = count + if not connceted.(i) && List.is_empty con then 1 else 0 in

      unconnected count (i+1) rest
  in
  unconnected 0 0 s

let () =
  for _ = 1 to 100 do
    let h = Scanf.scanf " %s" ident |> String.to_list in
    let unconnected = unconnected h in
    Printf.printf "%d\n" @@ n - 1 - unconnected;
    Out_channel.(flush stdout)
  done
