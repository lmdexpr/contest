open Core
open Scanf

module Point = struct
  type t = { x : int; y : int }

  let create x y = { x; y }
  
  let input () = scanf " %d %d" create
  
  let cross p q r = (q.x - p.x) * (r.y - p.y) - (q.y - p.y) * (r.x - q.x)

  let ( = ) p1 p2 = p1.x = p2.x && p1.y = p2.y

  let dist p1 p2 =
    let dx = float (p1.x - p2.x) in
    let dy = float (p1.y - p2.y) in
    sqrt (dx *. dx +. dy *. dy)
end

module Conveyor = struct
  type t = {
    from : Point.t;
    to_  : Point.t;
  }

  let intersect { from = p1; to_ = p2 } { from = q1; to_ = q2 } =
    let sign x =
      if x > 0 then 1 
      else if x < 0 then -1 
      else 0 
    in
    not Point.(p1 = q1 || p1 = q2 || p2 = q1 || p2 = q2) && (
      let d1 = sign (Point.cross q1 q2 p1) in
      let d2 = sign (Point.cross q1 q2 p2) in
      let d3 = sign (Point.cross p1 p2 q1) in
      let d4 = sign (Point.cross p1 p2 q2) in
      d1 * d2 < 0 && d3 * d4 < 0
    )
end

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id
let k = scanf " %d" Fn.id

let processor_sites = Array.init n ~f:(fun i -> i, Point.input ())
let sorter_sites    = Array.init m ~f:(fun i -> i, Point.input ())

let prob = Array.init k ~f:(fun _ -> Array.init n ~f:(fun _ -> scanf " %f" Fn.id))

module BinaryTree = struct
  type t =
    [ `Leaf of int
    | `Node of int * t * t
    ]

  type extended = [ t | `Inlet ]

  let number = function
    | `Leaf i         -> i
    | `Node (i, _, _) -> n + i

  let _position = function
    | `Leaf i         -> processor_sites.(i)
    | `Node (i, _, _) -> sorter_sites.(i)

  let traverse ~leaf ~node =
    let rec traverse ~acc parent tree =
      match tree with
      | `Leaf _                as tree -> leaf ~acc parent tree
      | `Node (_, left, right) as tree ->
        let lacc, racc = node ~acc parent tree in
        traverse ~acc:lacc tree left;
        traverse ~acc:racc tree right;
    in
  traverse
  
end

let build_binary_tree p0 =
  let epsilon = 10 in
  let rec aux convs parent left ssites = function
    | []                -> left
    | (k, pp) :: psites ->
      let ssites, remains = List.partition_tf ssites ~f:(fun (_, Point.{ x; _ }) -> x < pp.Point.x + epsilon) in
      match
        ssites
        |> List.sort ~compare:(fun (_, p1) (_, p2) ->
          let d1 = Point.dist pp p1 in
          let d2 = Point.dist pp p2 in
          Float.compare d1 d2
        )
        |> List.filter ~f:(fun (_, q) ->
          not @@ List.exists convs ~f:(Conveyor.intersect { from = parent; to_ = q })
        )
      with
      | []           -> `Leaf k
      | (l, sp) :: _ -> 
        let convs =
          Conveyor.{ from = parent; to_ = sp } ::
          Conveyor.{ from = sp;     to_ = pp } ::
          convs
        in
        let left = `Leaf k in
        `Node (l, left, aux convs sp left remains psites)
  in
  Array.to_list processor_sites
  |> List.sort ~compare:(fun (_, Point.{ x = x1; _ }) (_, Point.{ x = x2; _ }) -> Int.ascending x1 x2)
  |> aux [] p0 (`Leaf 0) Array.(to_list sorter_sites)

module Sorter_config = struct
  type t =
    | No
    | On of int * int * int

  let no = No

  let on k l r = On (k, BinaryTree.number l, BinaryTree.number r)

  let print = function
    | No             -> printf "-1\n"
    | On (k, v1, v2) -> printf "%d %d %d\n" k v1 v2
end

let arrangement tree =
  let now_prob = Array.init n ~f:(fun i -> i, 1.0) in

  let processor_assignment = Array.create ~len:n (-1) in
  let sorter_config        = Array.create ~len:m Sorter_config.no in

  BinaryTree.traverse (`Inlet : BinaryTree.extended) tree ~acc:now_prob
    ~leaf:(fun ~acc _ (`Leaf i) ->
      Array.max_elt acc ~compare:(fun (_, p) (_, q) -> Float.compare p q)
      |> Option.value ~default:(-1, 0.0)
      |> fun (k, _) ->
      processor_assignment.(i) <- k
    )
    ~node:(fun ~acc _parent -> function 
      | `Node (i, (`Leaf _ as left), right) ->
        let rights = Array.map prob ~f:(fun prob ->
          Array.init n ~f:(fun j -> j, prob.(j) *. snd acc.(j))
        ) in
        let k, _ =
          Array.mapi rights ~f:Tuple2.create
          |> Array.min_elt ~compare:(fun (_, l) (_, r) ->
            let f (_, p) = Float.(0.7 < p) in
            Int.compare
              Array.(count l ~f)
              Array.(count r ~f)
          )
          |> Option.value_exn
        in
        sorter_config.(i) <- Sorter_config.on k left right;
        let left_prob = Array.map acc ~f:(fun (j, p) -> j, p *. (1. -. prob.(k).(j))) in
        left_prob, rights.(k)
      | _ -> 
        failwith "Unexpected tree structure"
    );

  let rec fill_processor_assignment used i =
    if n <= i then ()
    else (
      if Set.mem used processor_assignment.(i) then 
        processor_assignment.(i) <- Iter.(0 -- pred n) |> Iter.find_pred_exn (fun j -> not @@ Set.mem used j);

      fill_processor_assignment (Set.add used processor_assignment.(i)) (i + 1)
    )
  in
  fill_processor_assignment Int.Set.empty 0;

  processor_assignment, BinaryTree.number tree, sorter_config

let () =
  let inlet_pos = Point.create 0 5000 in
  let tree = build_binary_tree inlet_pos in

  let processor_assignment, inlet_dest, sorter_config = arrangement tree in

  Array.iter processor_assignment ~f:(printf "%d ");
  printf "\n";
  printf "%d\n" inlet_dest;
  Array.iter sorter_config ~f:Sorter_config.print
