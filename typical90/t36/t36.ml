open Core

let n, q = Scanf.scanf "%d %d" Tuple2.create

let rotate45 x y = x - y, x + y
let ps = Array.init n ~f:(fun _ -> Scanf.scanf " %d %d" rotate45)

let inf = 1000000001
let x_min, y_min, x_max, y_max =
  Array.fold ps
    ~init:(inf, inf, -inf, -inf)
    ~f:(fun (x_min, y_min, x_max, y_max) (x, y) ->
        min x x_min, min y y_min, max x x_max, max y y_max
    )

let () =
  for _ = 1 to q do
    let q = Scanf.scanf " %d" @@ fun i -> i - 1 in
    let x, y = ps.(q) in
    printf "%d\n%!" @@ max (max (x - x_min) (x_max - x)) (max (y - y_min) (y_max - y))
  done
