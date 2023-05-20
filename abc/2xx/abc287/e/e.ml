open Core
open Scanf

let n = scanf "%d" ident

let s = Array.init n ~f:(fun _ -> scanf " %s" @@ fun s -> String.length s, s)

let ans = Array.init n ~f:(const @@ -1)
let rec solve ?(k=0) targets =
  if Array.is_empty targets then ()
  else
    let t, f = Array.partition_tf targets ~f:(fun i -> Tuple2.get1 s.(i) = k) in
    Array.iter t ~f:(fun i -> ans.(i) <- k);
    let next = Array.init 26 ~f:(const []) in
    Array.iter f ~f:(fun i ->
        let ord = Char.to_int (Tuple2.get2 s.(i)).[k] - Char.to_int 'a' in
        next.(ord) <- i :: next.(ord)
      );
    Array.iter next ~f:(function
        | []  -> ()
        | [i] -> ans.(i) <- k
        | is  -> solve ~k:(k+1) @@ Array.of_list is
      )

let () =
  solve @@ Array.init n ~f:ident;
  Array.iter ans ~f:(printf "%d\n%!")
