open Core
open Scanf

let s, t = scanf "%s %s" Tuple2.create

let string_cut s n =
  let rec loop acc = function
    | [] -> acc
    | xs -> 
      let a, b = List.split_n xs n in
      loop (List.to_array a :: acc) b
  in
  loop [] String.(to_list s)

let () =
  let t = String.rev t in
  for w = 1 to String.length s - 1 do
    let s = string_cut s w in
    for c = 1 to w do
      let s = 
        List.filter_map s ~f:(fun s -> 
          try Some s.(c - 1) with _ -> None
        ) |> String.of_char_list 
      in
      if String.(s = t) then (
        printf "Yes\n";
        exit 0
      )
    done
  done

let () = printf "No\n"
