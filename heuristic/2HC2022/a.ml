open Core

let t_max = Scanf.scanf "%d" ident

let scan1 _ = Scanf.scanf " %d" ident
let scan2 _ = Scanf.scanf " %d %d" Tuple2.create
let scan3 _ = Scanf.scanf " %d %d %d" Tuple3.create

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)
  let connect g v u d = push g v (u, d); push g u (v, d)

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let v, e = scan2 ()
let g = Graph.create ~size:v (module Int)
let () =
  for _ = 1 to e do
    let v, u, d = scan3 () in
    Graph.connect g v u d
  done

let n_workers = scan1 ()
module Worker = struct
  type t = { id : int; pos: int; limit_task : int; job_types : int array }
  let input id : t =
    let id = id + 1 in
    let pos, limit_task, jobs = scan3 () in
    let job_types = Array.init jobs ~f:scan1 in
    { id; pos; limit_task; job_types }
  let stay = "stay"
  let move = sprintf "move %d"
  let exec = sprintf "execute %d %d"
end
let workers = Array.init n_workers ~f:Worker.input

module SI = Set.Make(Int)

let n_jobs = scan1 ()
module Job = struct
  type t = { id : int; type_ : int; tasks : int; reward : int -> int; depends : SI.t }
  let input () : int * t =
    let id = scan1 () in
    let type_, tasks, position = scan3 () in
    let n = scan1 () in
    let points = Array.init n ~f:scan2 in
    let reward t = 
      let t1, y1 = points.(0) in
      let tn, yn = points.(n - 1) in
      if t < t1 then y1
      else if t >= tn then yn
      else 
        let compare (tl, _) (tr, _) = compare tl tr in
        let next =
          Array.binary_search ~compare points `First_strictly_greater_than (t,0)
          |> Option.value ~default:(n-1)
        in
        let (t_prev, y_prev) = points.(next - 1) in
        let (t_next, y_next) = points.(next) in
        (y_next - y_prev) * (t - t_prev) / ((t_next - t_prev) + y_prev)
    in
    let depends = SI.of_array @@ Array.init (scan1 ()) ~f:scan1 in
    position, { id; type_; tasks; reward; depends }
end
let jobs = Array.init (v + 1) ~f:(const [])
let () =
  for _ = 1 to n_jobs do
    let position, job = Job.input () in
    jobs.(position) <- job :: jobs.(position)
  done

let actions = Array.init t_max ~f:(fun _ -> Array.init n_workers ~f:(const Worker.stay))

let () = Array.iter actions ~f:(Array.iter ~f:(printf "%s\n%!"))
let _s = scan1 ()
