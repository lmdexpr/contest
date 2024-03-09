open Core
open Scanf

let a = Array.create ~len:64 0
let () =
  a.(0) <- scanf "%d" ident;
  for i = 1 to 63 do
    a.(i) <- scanf " %d" ident;
  done

let ans =
  Array.rev_inplace a;
  let open Big_int in
  Array.fold a ~init:zero_big_int ~f:(fun acc a ->
    mult_int_big_int 2 acc |> add_int_big_int a
    )
  |> string_of_big_int

let () = printf "%s\n%!" ans
