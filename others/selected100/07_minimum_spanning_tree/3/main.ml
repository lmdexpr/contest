open Scanf
open Printf

module Set = Set.Make(struct type t = int let compare = compare end)
module List = struct
  include List
  let init n f = Array.to_list @@ Array.init n f
end

type cell = {x : float; y : float; z : float; r : float;}

let solve n =
  let cells = Array.init n (fun _ -> 
    scanf " %f %f %f %f" @@ fun x y z r -> {x; y; z; r;}
  )
  in

  let dist c1 c2 =
    let dx = c1.x -. c2.x in
    let dy = c1.y -. c2.y in
    let dz = c1.z -. c2.z in
    let d = sqrt (dx *. dx +. dy *. dy +. dz *. dz) in
    max 0. (d -. (c1.r +. c2.r))
  in

  let es = Array.init n (fun i ->
    Array.init n (fun j -> (if i = j then 0. else dist cells.(i) cells.(j)), j)
  )
  in

  let rec prim ans choose remains =
    if Set.is_empty remains then ans
    else (
      let d, j =
        Set.fold (fun i acc ->
          Array.fold_left (fun acc -> function
            | (d, j) when Set.mem j remains && d < fst acc -> d, j
            | _ -> acc
          ) acc es.(i)
        ) choose (max_float, -1)
      in
      let remains = Set.remove j remains in
      let choose = Set.add j choose in
      prim (ans +. d) choose remains
    )
  in

  prim 0. Set.(of_list [ 0 ]) Set.(of_list @@ List.init (n - 1) succ)
  |> printf "%.3f\n%!"

let () =
  let n = ref 0 in
  scanf "%d\n" @@ fun x -> n := x;

  while !n <> 0 do
    solve !n;
    scanf " %d" @@ fun x -> n := x;
  done
