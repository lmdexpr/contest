open Core

let n, p, k = Scanf.scanf "%d %Ld %Ld" Tuple3.create

let a = Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ -> Scanf.scanf " %Ld" ident))

let warshall_floyd dist =
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        let open Int64 in
        dist.(i).(j) <- min dist.(i).(j) (dist.(i).(k) + dist.(k).(j))
      done
    done
  done

let calc x =
  let dist = Array.map a ~f:( Array.map ~f:Int64.(fun a -> if a = -1L then x else a) ) in
  warshall_floyd dist;
  Iter.(0 -- (n - 1))
  |> Iter.map (fun i -> Iter.(i + 1 -- (n - 1)) |> Iter.filter_count Int64.(fun j -> dist.(i).(j) <= p))
  |> Iter.sum
  |> Int64.of_int

open Int64

let rec binsearch ~ok left right =
  if abs (right - left) <= 1L then right
  else
    let mid = (right + left) / 2L in
    let left, right = if ok mid then left, mid else mid, right in
    binsearch ~ok left right

let () =
  if calc (1L lsl 60) = k then printf "Infinity\n%!"
  else
    let l = binsearch 0L (1L lsl 60) ~ok:(fun x -> calc x <= k)
    and r = binsearch 0L (1L lsl 60) ~ok:(fun x -> calc x <  k)
    in
    printf "%Ld\n%!" @@ r - l
