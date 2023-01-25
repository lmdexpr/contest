open Core

let h, w = Scanf.scanf "%d %d" Tuple2.create

let c = Array.init h ~f:(fun _ -> Scanf.scanf " %s" ident)
let is_path x y = Char.(c.(x).[y] = '.')

let backtrack sx sy =
  let (>|^) iter f = Iter.filter f iter in
  let visited = Array.make_matrix ~dimx:h ~dimy:w false in
  let is_loop x y = sx = x && sy = y in
  let rec dfs x y =
    if is_loop x y && visited.(x).(y) then 0
    else begin
      visited.(x).(y) <- true;
      Iter.of_list [x + 1, y; x, y + 1; x - 1, y; x, y - 1]
      >|^ (fun (x, _) -> 0 <= x && x < h)
      >|^ (fun (_, y) -> 0 <= y && y < w)
      >|^ (Tuple2.uncurry is_path)
      >|^ (fun (x, y) -> is_loop x y || not visited.(x).(y))
      |> Iter.map (fun (x, y) -> 1 + dfs x y)
      |> Iter.max ~lt:(<)
      |> Option.value ~default:(-500)
    end
  in
  dfs sx sy

let (let+) x k = Iter.flat_map k x
let (let^) x k = Iter.filter_map k x
let ans =
  Iter.(
    let+ x = 0 -- (h - 1) in
    let^ y = 0 -- (w - 1) in
    Option.some_if (is_path x y) (backtrack x y)
  )
  |> Iter.max_exn ~lt:(<)
let ans = if ans > 2 then ans else -1

let () = printf "%d\n%!" ans
