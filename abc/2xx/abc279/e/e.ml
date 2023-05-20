open Core

let n, m = Scanf.scanf "%d %d" Tuple2.create
let a = Array.init m ~f:(fun _ -> Scanf.scanf " %d" ident)

let b = Array.init (n+1) ~f:ident
let () = Array.iter a ~f:(fun i -> Array.swap b i (i+1))

let pos = Array.init (n+1) ~f:(const 0)
let () = Array.iteri b ~f:(fun i b -> pos.(b) <- i)

let b = Array.init (n+1) ~f:ident
let () =
  let f i = 
    let ans_i = 
      match b.(i), b.(i+1) with
      | 1, _ -> b.(i+1)
      | _, 1 -> b.(i)
      | _, _ -> 1
    in
    printf "%d\n%!" pos.(ans_i);
    Array.swap b i (i+1)
  in
  Array.iter a ~f
