open Core
open Scanf

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.snoc acc v
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v

  let around g v = find g v |> Option.value ~default:Iter.empty
end

(* Input *)

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id
let q = scanf " %d" Fn.id
let l = scanf " %d" Fn.id
let _w = scanf " %d" Fn.id

let g = Array.init m ~f:(fun _ -> scanf " %d" Fn.id)

let lx = Array.create ~len:n 0.
let ly = Array.create ~len:n 0.
let rx = Array.create ~len:n 0.
let ry = Array.create ~len:n 0.
let () =
  for i = 0 to n - 1 do
    scanf " %f" @@ Array.set lx i; scanf " %f" @@ Array.set rx i;
    scanf " %f" @@ Array.set ly i; scanf " %f" @@ Array.set ry i;
  done

let q = ref q
let query c =
  let n = Iter.length c in
  if !q <= 0 || n <= 1 then []
  else (
    decr q;
    printf "? %d" n; Iter.iter (printf " %d") c; printf "\n%!";
    List.init (n - 1) ~f:(fun _ -> scanf " %d %d" Tuple2.create)
  )

let med_x i = (lx.(i) +. rx.(i)) /. 2.
let med_y i = (ly.(i) +. ry.(i)) /. 2.

let estimated = Array.init n ~f:(fun i -> med_x i, med_y i)

let dist = Array.make_matrix ~dimx:n ~dimy:n 0.
let update_dist () =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let xi, yi = estimated.(i) in
      let xj, yj = estimated.(j) in
      let d = 
        let open Float in
        sqrt @@
        (xi - xj) * (xi - xj) + (yi - yj) * (yi - yj)
      in
      dist.(i).(j) <- d
    done
  done
let () = update_dist ()

let neighbors = Array.init n ~f:(fun i ->
  Iter.(0 -- (n - 1)) 
  |> Iter.filter (fun j -> i <> j)
  |> Iter.sort_uniq ~cmp:(fun a b -> Float.ascending dist.(i).(a) dist.(i).(b))
)

let graph  = Graph.create ~size:n (module Int)
let degree = Array.create ~len:n 0
let value   = Array.make_matrix ~dimx:n ~dimy:n 0

let connect ~f u v = (
  Graph.connect graph u v;
  degree.(u) <- degree.(u) + 1;
  degree.(v) <- degree.(v) + 1;
  value.(u).(v) <- f u v;
  value.(v).(u) <- f v u;
  v
)

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

let forces = Array.create ~len:n (0., 0.)

let clamp v min_v max_v = Float.max min_v (Float.min max_v v)

let update_estimated neighbors result =
  let alpha = 0.01 in
  let beta = 1000. in
  let epsilon = 1e-6 in
  neighbors |> Iter.iter @@ fun u ->
  neighbors |> Iter.iter @@ fun v ->
  if u < v then (
    let dx = fst estimated.(v) -. fst estimated.(u) in
    let dy = snd estimated.(v) -. snd estimated.(u) in
    let dist_sq = dx *. dx +. dy *. dy +. epsilon in
    let dist = sqrt dist_sq in
    let nx = dx /. dist in
    let ny = dy /. dist in
    let ux, uy = forces.(u) in
    let vx, vy = forces.(v) in
    if Set.mem result (u, v) then (
      let force_magnitude = alpha *. dist in
      forces.(u) <- ux +. force_magnitude *. nx, uy +. force_magnitude *. ny;
      forces.(v) <- vx -. force_magnitude *. nx, vy -. force_magnitude *. ny;
    ) else (
      let force_magnitude = beta /. dist in
      forces.(u) <- ux -. force_magnitude *. nx, uy -. force_magnitude *. ny;
      forces.(v) <- vx +. force_magnitude *. nx, vy +. force_magnitude *. ny;
    )
  );
  neighbors |> Iter.iter (fun i ->
    let fx, fy = forces.(i) in
    let x, y = estimated.(i) in
    estimated.(i) <- clamp (x +. fx) lx.(i) rx.(i), clamp (y +. fy) ly.(i) ry.(i);
  )

let () =
  let prev = ref 0 in
  while 0 < !q do
    let lt u v =
      if degree.(u) = degree.(v) then value.(!prev).(u) < value.(!prev).(v)
      else
        degree.(u) < degree.(v)
    in
    let i = Iter.(0 -- (n - 1)) |> Iter.min_exn ~lt in
    prev := i;
    let neighbors =
      neighbors.(i) 
      |> Iter.take (l - 1) |> Iter.cons i 
    in
    let result = query neighbors in
    List.iter result ~f:(fun (v, u) ->
      ignore @@ connect v u ~f:(fun v u -> value.(v).(u) + 1);
    );
    result
    |> List.map ~f:(fun (v, u) -> min v u, max v u)
    |> SP.of_list
    |> update_estimated neighbors;
  done

let () = update_dist ()

let () =
  for v = 0 to n - 1 do
    neighbors.(v) |> Iter.iteri (fun c u ->
      if value.(v).(u) <= 0 || value.(u).(v) <= 0 then
        ignore @@ connect v u ~f:(fun v u -> (value.(v).(u) - c) / 2)
    )
  done

type answer = {
  group : Int.Set.t;
  edges : (int * int) list;
}

let score p v = value.(p).(v) - Float.to_int dist.(p).(v) / 250

module ValueSet2 = Set.Make(struct
  type t = int * int
  let compare (p1, v1) (p2, v2) = Int.compare (score p1 v1) (score p2 v2)
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end)

let group_by ~graph ~used start k =
  let rec loop { group; edges } ?(candidates=ValueSet2.empty) p =
    if Set.length group = k then { group; edges }
    else
      let p_around =
        Graph.around graph p
        |> Iter.filter (fun u -> not used.(u))
        |> Iter.map (fun v -> p, v)
        |> Iter.fold Set.add ValueSet2.empty
      in
      let candidates = Set.union candidates p_around in
      let p, v = Set.max_elt_exn candidates in
      used.(v) <- true;
      let candidates = Set.filter candidates ~f:(fun (_, v) -> not used.(v)) in
      let group = Set.add group v in
      let edges = (p, v) :: edges in
      loop ~candidates { group; edges } v
  in
  used.(start) <- true;
  loop { group = Int.Set.singleton start; edges = [] } start

let compress used v group =
  Set.to_list group
  |> List.filter_map ~f:(fun v ->
    used.(v) <- true;
    Graph.around graph v 
    |> Iter.filter (fun u -> not used.(u))
    |> Iter.max ~lt:(fun u w -> score v u < score v w)
  )
  |> List.fold ~init:v ~f:(connect ~f:(fun u w -> (value.(v).(u) + value.(v).(w)) / 2))

let jam = Array.map dist ~f:(Array.fold ~init:(0,0,0,0) ~f:(fun (x1,x2,x3,x4) -> 
  let (<) = Float.(<) in
  function
  | d when d <  50. -> x1 + 1, x2, x3, x4
  | d when d < 100. -> x1, x2 + 1, x3, x4
  | d when d < 150. -> x1, x2, x3 + 1, x4
  | d when d < 200. -> x1, x2, x3, x4 + 1
  | _              -> x1, x2, x3, x4
))

let compare_jam u v =
  let score v = 
    let x1, x2, x3, x4 = jam.(v) in
    degree.(v) + x1 + x2 / 2 + x3 / 4 + x4 / 8
  in
  Int.ascending (score u) (score v)

let answers = Array.create ~len:m { group = Int.Set.empty; edges = [] }
let () = 
  let used = Array.create ~len:n false in
  let start = ref 0 in
  List.init m ~f:Fn.id
  |> List.sort ~compare:(fun i j -> Int.descending g.(i) g.(j))
  |> List.iter ~f:(fun i ->
    while used.(!start) do
      incr start;
    done;
    let v = 
      Iter.(!start -- (n - 1))
      |> Iter.sort ~cmp:compare_jam
      |> Iter.find_pred_exn (fun v -> not used.(v))
    in
    let { group; edges } = group_by ~graph ~used v g.(i) in
    compress used v group |> ignore;
    answers.(i) <- { group; edges }
  )

let () =
  printf "!\n";
  Array.iter answers ~f:(fun { group; edges } ->
    Set.iter  group ~f:(printf "%d "); printf "\n%!";
    List.iter edges ~f:(Tuple2.uncurry @@ printf "%d %d\n");
  )
