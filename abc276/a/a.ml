open Core

let s = In_channel.(input_line_exn stdin) |> String.to_list

let rec f a i = function
  | 'a' :: s -> f i (i+1) s
  | _ :: s   -> f a (i+1) s
  | _        -> a+1

let () = Out_channel.printf "%d\n" @@ f (-2) 0 s
