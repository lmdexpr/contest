open Core

let _n, s = Scanf.scanf "%d %s" Tuple2.create

let rec solve ?(wrapped=false) = function
  | []        -> ()
  | '"' :: xs -> printf "\""; solve ~wrapped:(not wrapped) xs
  | ',' :: xs -> printf "%c" @@ if wrapped then ',' else '.'; solve ~wrapped xs
  | x :: xs   -> printf "%c" x; solve ~wrapped xs

let () =
  String.to_list s |> solve;
  printf "\n%!"
