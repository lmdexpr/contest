open Core
open Scanf

let dijkstra size a = 
  let inf = 1000_000_001L in
  let dist = Array.init (size+1) ~f:(const inf) in dist.(0) <- 0L;
  let rec dijkstra i set =
    if size < i then ()
    else
      match Int64.Set.min_elt set with
      | None           -> ()
      | Some v ->
        let set = Int64.Set.remove set v in
        if Int64.(dist.(Int.pred i) = v) then dijkstra i set
        else begin
          dist.(i) <- v;
          Array.fold a ~init:set ~f:(fun set a -> Int64.Set.add set Int64.(a + v))
          |> dijkstra (i + 1)
        end
  in
  dijkstra 1 (Int64.Set.of_array a);
  dist.(size)

let n, k = scanf "%d %d" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %Ld" ident)

let ans = dijkstra k a

let () = printf "%Ld\n%!" ans
