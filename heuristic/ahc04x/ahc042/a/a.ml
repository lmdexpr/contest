open Core
open Scanf

let n, m = scanf " %d %d" Tuple2.create
let k, t = scanf " %d %d" Tuple2.create

let home = Array.create ~len:m (0, 0)
let work = Array.create ~len:m (0, 0)
let () =
  for i = 0 to m - 1 do
    home.(i) <- scanf " %d %d" Tuple2.create;
    work.(i) <- scanf " %d %d" Tuple2.create;
  done

type rail = int

module Section = struct
  type t =
    | Empty
    | Rail of rail
    | Station

  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
  let sexp_of_t = function
    | Empty   -> Sexp.Atom "Empty"
    | Rail r  -> Sexp.List [ Sexp.Atom "Rail"; sexp_of_int r ]
    | Station -> Sexp.Atom "Station"

  let t_of_sexp = function
    | Sexp.Atom "Empty"   -> Empty
    | Sexp.Atom "Station" -> Station
    | Sexp.List [ Sexp.Atom "Rail"; ] -> Rail 0
    | _ -> assert false
end

let grids = Array.make_matrix ~dimx:n ~dimy:n Section.Empty

type command = [
  | `Put of (rail * (int * int))
  | `Arrange of (int * int)
  | `Wait
  ]

let cost_of_command = function
  | `Put _     -> 100
  | `Arrange _ -> 5000
  | `Wait      -> 0

let built_rail (i, j) r =
  match grids.(i).(j) with
  | Empty -> grids.(i).(j) <- Section.Rail r; Ok (Section.Empty, `Put (r, (i, j)))
  | sec   -> Error sec

let built_station (i, j) = 
  match grids.(i).(j) with
  | Station -> Error Section.Station
  | sec     -> grids.(i).(j) <- Section.Station; Ok (sec, `Arrange (i, j))

let solve _i =
  `Wait

let () =
  for i = 1 to t do
    match solve i with
    | `Put (r, (x, y)) -> printf "%d %d %d\n%!" r x y
    | `Arrange (x, y)  -> printf "0 %d %d\n%!" x y
    | `Wait            -> printf "-1\n%!"
  done
