open Core

let n = Scanf.scanf "%d" ident
let p = List.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let rec split_while_sorted acc = function
  | []       -> acc, []
  | hd :: tl ->
    if List.hd acc |> Option.exists ~f:(fun pred -> pred < hd) then hd :: acc, List.rev tl
    else
      split_while_sorted (hd :: acc) tl

let second, first = split_while_sorted [] p

let thr, second = List.hd_exn second, List.tl second |> Option.value ~default:[]

let sml, lrg = List.partition_tf second ~f:(fun e -> e <= thr)
let sml = List.sort ~compare:Int.descending sml

let t, sml = List.hd sml |> Option.to_list, List.tl sml |> Option.value ~default:[]

let print_list lst = List.iter lst ~f:(fun e -> Printf.printf "%d " e)

let () =
  print_list first;
  print_list t;
  print_list @@ List.sort ~compare:Int.descending lrg;
  Printf.printf "%d " thr;
  print_list sml;
  Out_channel.newline stdout
