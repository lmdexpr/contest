open Core
open Scanf

let n = scanf "%d" Fn.id

let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let pos = Array.create ~len:(n+1) 0
let () =
  for i = 0 to n - 1 do
    pos.(a.(i)) <- i
  done

let rec solve ?(acc=[]) x =
  if n <= x then List.rev acc
  else
    let y = pos.(x) + 1 in
    if x = y then solve ~acc (x + 1)
    else (
      a.(y - 1) <- a.(x - 1); pos.(a.(x - 1)) <- y - 1;
      a.(x - 1) <- x; pos.(x) <- x - 1;
      solve ~acc:((min x y, max x y) :: acc) (x + 1)
    )

let ans = solve 1 

let () =
  printf "%d\n" (List.length ans);
  List.iter ans ~f:(Tuple2.uncurry @@ printf "%d %d\n")
