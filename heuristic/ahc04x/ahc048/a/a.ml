open Core
open Scanf

module PI = struct
  type t = int * int
  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = Tuple2.sexp_of_t sexp_of_int sexp_of_int
  let t_of_sexp = Tuple2.t_of_sexp int_of_sexp int_of_sexp
end
module SP = Set.Make(PI)

module Color = struct
  type t = { c : float; m : float; y : float }

  let input () = scanf " %f %f %f" (fun c m y -> { c; m; y })
  
  let white = { c = 0.0; m = 0.0; y = 0.0 }
  let black = { c = 1.0; m = 1.0; y = 1.0 }

  let (+) c1 c2 = { c = c1.c +. c2.c; m = c1.m +. c2.m; y = c1.y +. c2.y }
  let (-) c1 c2 = { c = c1.c -. c2.c; m = c1.m -. c2.m; y = c1.y -. c2.y }

  let ( *$ ) s c = { c = s *. c.c; m = s *. c.m; y = s *. c.y }
  let ( /$ ) c s =
    if Float.(s = 0.0) then black 
    else
      { c = c.c /. s; m = c.m /. s; y = c.y /. s }

  let mix v1 c1 v2 c2 =
    let total = v1 +. v2 in
    (v1 /. total) *$ c1 + (v2 /. total) *$ c2
  
  let distance c1 c2 =
    let diff = c2 - c1 in
    Float.sqrt @@
    diff.c *. diff.c +. diff.m *. diff.m +. diff.y *. diff.y
end

module Cluster = struct
  type centroids = Color.t array
  type t = {
    size : int;
    centroids : centroids;
    assignments : int array;
    colors : Color.t array;
    frequency : int array;
  }

  let initialize_centroids k colors : centroids =
    let n = Array.length colors in
    Random.self_init ();
    let centroids = Array.init k ~f:(const Color.white) in
    let rec choose chosen count =
      if k <= count then centroids
      else (
        let i = Random.int n in
        if Set.mem chosen i then choose chosen count
        else (
          centroids.(count) <- colors.(i);
          choose Set.(add chosen i) (count + 1)
        )
      )
    in
    choose Int.Set.empty 0

  let frequency { assignments; size; _; } =
    let counts = Array.create ~len:size 0 in
    Array.iter assignments ~f:(fun i -> counts.(i) <- counts.(i) + 1);
    counts

  let kmeans k ?(limit=100) ?(tolerance=1e-9) colors =
    let n = Array.length colors in
    let assignment ({ centroids; assignments; colors; _; } as cluster) =
      Array.iteri colors ~f:(fun i c ->
        let _, best =
          Array.foldi centroids ~init:(Float.infinity, 0) ~f:(fun i (acc, best) centroid ->
            let d = Color.distance c centroid in
            if Float.(d < acc) then d, i
            else
              acc, best
          )
        in
        assignments.(i) <- best;
      );
      let cluster = { cluster with assignments; } in
      { cluster with frequency = frequency cluster; }
    in
    let update ({ centroids; assignments; size; _; } as cluster) =
      let new_centroids = Array.copy centroids in

      let cluster_sums   = Array.init size ~f:(const Color.white) in
      let cluster_counts = Array.create ~len:size 0 in

      Array.iteri colors ~f:(fun i c ->
        let cluster_idx = assignments.(i) in
        cluster_sums.(cluster_idx)   <- Color.(cluster_sums.(cluster_idx) + c);
        cluster_counts.(cluster_idx) <- cluster_counts.(cluster_idx) + 1
      );
      let centroid_movement =
        Iter.(0 -- pred size)
        |> Fn.flip Iter.fold 0.0 (fun m i ->
          if 0 < cluster_counts.(i) then (
            let centroid = Color.(cluster_sums.(i) /$ float cluster_counts.(i)) in
            let movement = Color.distance centroids.(i) centroid in
            new_centroids.(i) <- centroid;
            Float.max m movement
          ) else (
            new_centroids.(i) <- colors.(Random.int n);
            Float.infinity
          )
        )
      in
      Float.(centroid_movement < tolerance),
      { cluster with centroids = new_centroids; }
    in
    let rec loop i (converged, cluster) =
      if limit <= i then eprintf "Warning: k-means did not converge within the limit of %d iterations.\n" limit;
      if converged || limit <= i then cluster
      else
        cluster
        |> assignment
        |> update 
        |> loop (i + 1)
    in
    loop 0 (
      false, 
      { colors;
        centroids = initialize_centroids k colors;
        assignments = Array.create ~len:n 0;
        size = k;
        frequency = [| |];
      }
    )
end

module Well = struct
  type t = {
    cells : SP.t;
    color : Color.t;
    volume : float;
    capacity : float;
  }

  let create cells capacity = { cells; color = Color.white; volume = 0.0; capacity; }

  let cell_volume { volume; capacity; _; } =
    if Float.(capacity = 0.0) then 0.0
    else 
      Float.(volume / capacity)

  let representative { cells; _; } =
    match Set.min_elt cells with
    | Some (i, j) -> (i, j)
    | None ->
      failwith "No representative found in empty well"
end

module Palette = struct
  type rectangle = { x: int; y: int; width: int; height: int }
  
  type t = {
    n : int;
    well_map : int array array;
    wells : Well.t array;
  }

  let v_barriers { well_map; _; } i j = well_map.(i).(j) <> well_map.(i).(j+1)
  let h_barriers { well_map; _; } i j = well_map.(i).(j) <> well_map.(i+1).(j)

  let initial_print p =
    for i = 0 to p.n - 1 do
      for j = 0 to p.n - 2 do
        printf "%d " @@ Bool.to_int @@ v_barriers p i j;
      done;
      printf "\n";
    done;
    for i = 0 to p.n - 2 do
      for j = 0 to p.n - 1 do
        printf "%d " @@ Bool.to_int @@ h_barriers p i j;
      done;
      printf "\n";
    done;
    printf "%!"

  let grids n g =
    Array.init (g * g) ~f:(fun i ->
      let width = n / g in
      {
        x = (i / g) * width;
        y = (i % g) * width;
        width;
        height = width;
      }
    )

  let layout n _cluster =
    let g = 4 in
    let rectangles = grids n g in

    let well_map = Array.make_matrix ~dimx:n ~dimy:n (-1) in

    let wells = Array.mapi rectangles ~f:(fun idx { x; y; width; height; } ->
      let cells =
        Iter.(x -- pred (x + height))
        |> Iter.flat_map (fun i ->
          Iter.(y -- pred (y + width)) |> Iter.map (fun j -> (i, j))
        )
        |> Iter.fold (fun cells (i, j) ->
          well_map.(i).(j) <- idx;
          Set.add cells (i, j)
        )
          SP.empty
      in
      Well.create cells (float @@ (n / g) * (n / g))
    ) in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if well_map.(i).(j) = -1 then
          let nearest_well = ref 0 in
          let min_dist = ref Int.max_value in
          Array.iteri rectangles ~f:(fun idx rect ->
            let dist = abs (i - rect.x) + abs (j - rect.y) in
            if dist < !min_dist then (
              min_dist := dist;
              nearest_well := idx
            )
          );
          well_map.(i).(j) <- !nearest_well
      done
    done;
    
    { n; well_map; wells; }

  let add_operations = ref 0

  let add { wells; _; } i tubes k =
    incr add_operations;
    let w = wells.(i) in
    wells.(i) <- { w with 
      color  = Color.mix w.volume w.color 1. tubes.(k);
      volume = Float.(w.volume + 1.0);
    };
    let i, j = Well.representative wells.(i) in
    printf "1 %d %d %d\n" i j k

  let deliver { wells; _; } i =
    wells.(i) <- { wells.(i) with
      volume = Float.(wells.(i).volume - 1.0);
    };
    let i, j = Well.representative wells.(i) in
    printf "2 %d %d\n" i j

  let _drop { wells; _; } i =
    wells.(i) <- { wells.(i) with
      color  = Color.white;
      volume = 0.0;
    };
    let i, j = Well.representative wells.(i) in
    printf "3 %d %d\n" i j

  let _change_cell p old idx =
    if Set.length p.wells.(old).cells <= 2 then (
      invalid_arg "change_cell: cannot change cell in a well with only one cell left";
    );
    let i, j = Well.representative p.wells.(old) in
    if 0 < old && old < p.n then (
      let from = p.wells.(old) in

      p.well_map.(i).(j) <- idx;

      let cell_vol = Float.(from.volume / from.capacity) in
      p.wells.(old) <- { from with
        capacity = Float.(from.capacity - 1.);
        volume   = Float.(cell_vol * (from.capacity - 1.));
        cells    = Set.remove from.cells (i, j);
      };
      if 0 < idx && idx < p.n then (
        let to_  = p.wells.(idx) in
        p.wells.(idx) <- {
          capacity = Float.(to_.capacity + 1.);
          volume   = Float.(to_.volume + cell_vol);
          color    = Color.mix to_.volume to_.color cell_vol from.color;
          cells    = Set.add to_.cells (i, j);
        };
      );
      let around =
        [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1) ]
        |> List.filter ~f:(fun (k, l) -> 0 <= k && k < p.n && 0 <= l && l < p.n)
      in
      List.iter around ~f:(fun (k, l) ->
        let around = p.well_map.(k).(l) in
        if old = around && idx <> around then printf "4 %d %d %d %d\n" i j k l;
      );
      List.iter around ~f:(fun (k, l) ->
        let around = p.well_map.(k).(l) in
        if old <> around && idx = around then printf "4 %d %d %d %d\n" i j k l;
      )
    )

  let approach ({ wells; _; } as p) i tubes color d =
    if Float.(wells.(i).volume < 1.0) then (
      Array.foldi tubes ~init:(-1, Float.infinity) ~f:(fun idx (best_idx, best_dist) tube ->
        let dist = Color.distance color tube in
        if Float.(dist < best_dist) then (idx, dist)
        else (best_idx, best_dist)
      )
      |> fst
      |> add p i tubes
    );

    let current_distance = Color.distance color wells.(i).color in

    let max_depth, min_improvement, cell_volume_threshold, distance_threshold, improvement_ratio =
      if d > 2000 then 
        (2, 0.005, 0.02, 0.15, 0.85)
      else if d > 500 then
        (3, 0.003, 0.05, 0.1, 0.9)
      else
        (4, 0.001, 0.08, 0.05, 0.95)
    in

    let rec find_best_sequence depth best_distance well acc =
      if depth >= max_depth then acc
      else
        let is_empty = Iter.is_empty acc in
        match
        Array.mapi tubes ~f:(fun k tube ->
          let mixed = Color.mix well.Well.volume well.Well.color 1.0 tube in
          let new_distance = Color.distance color mixed in

          current_distance -. new_distance, new_distance, k,
          { well with
            color = mixed;
            volume = well.volume +. 1.0;
          }
        )
        |> Array.to_list
        |> List.sort ~compare:(fun (imp1, _, _, _) (imp2, _, _, _) -> Float.descending imp1 imp2)
        |> List.hd
        with
        | Some (imp, new_distance, k, new_well)
          when is_empty || Float.(min_improvement < imp && new_distance < best_distance *. improvement_ratio) ->

          find_best_sequence (depth + 1) new_distance new_well Iter.(snoc acc k)

        | _ -> acc
    in
    if Float.(wells.(i).volume < 1.0 || Well.cell_volume wells.(i) < cell_volume_threshold || distance_threshold < current_distance) then
      Iter.empty
      |> find_best_sequence 0 current_distance wells.(i)
      |> Iter.iter (add p i tubes);
end

(* --- Solver --- *)

let n = scanf " %d" Fn.id
let k = scanf " %d" Fn.id
let h = scanf " %d" Fn.id
let t = scanf " %d" Fn.id
let d = scanf " %d" Fn.id

let tubes   = Array.init k ~f:(fun _ -> Color.input ())
let targets = Array.init h ~f:(fun _ -> Color.input ())

let solve c =
  let cluster = Cluster.kmeans c targets in

  let palette = Palette.layout n cluster in
  Palette.initial_print palette;

  Palette.add_operations := 0;
  Array.iteri cluster.centroids ~f:(fun i c ->
    Palette.approach palette i tubes c d;
  );

  let total_distance = ref 0. in
  Array.iteri targets ~f:(fun k target ->
    let i = cluster.assignments.(k) in
    Palette.approach palette i tubes target d;
    total_distance := !total_distance +. Color.distance target palette.wells.(i).color;
    Palette.deliver palette i;
    cluster.frequency.(i) <- cluster.frequency.(i) - 1;
  );
  (1. +. float d *. (float !Palette.add_operations -. float h) +. 10000. *. !total_distance)

let () =
  eprintf "n = %d, k = %d, h = %d, t = %d, d = %d\n" n k h t d;
  eprintf "Using %d clusters (d=%d, k=%d)\n" 16 d k;
  eprintf "score= %f\n" @@ solve 16
