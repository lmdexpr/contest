open Core
open Scanf

let n = scanf "%d" Fn.id
let s = scanf " %s" String.to_array
let q = scanf " %d" Fn.id

let extract_index p =
  Array.filter_mapi s ~f:(fun i c -> Option.some_if (p c) i)
  |> Int.Set.of_array

let init = extract_index Char.is_uppercase, extract_index Char.is_lowercase

let full = Int.Set.of_array (Array.init n ~f:Fn.id)

let upper, lower =
  Array.init q ~f:(fun _ -> scanf " %d %d %c" Tuple3.create)
  |> Array.fold ~init ~f:(fun (upper, lower) ->
    function
    | (1, x, c) ->
      let x = x - 1 in
      s.(x) <- c;
      let upper = if Char.is_uppercase c then Set.add upper x else Set.remove upper x in
      let lower = if Char.is_lowercase c then Set.add lower x else Set.remove lower x in
      upper, lower
    | (2, _, _) -> Int.Set.empty, full
    | (3, _, _) -> full, Int.Set.empty
    | _ -> assert false
  )

let () =
  Set.iter lower ~f:(fun i -> s.(i) <- Char.lowercase s.(i));
  Set.iter upper ~f:(fun i -> s.(i) <- Char.uppercase s.(i));
  Array.iter s ~f:(fun c -> printf "%c" c);
  printf "\n"
