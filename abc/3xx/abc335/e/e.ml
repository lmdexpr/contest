open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
let a = Array.create ~len:(n+1) 0
let () =
  for i = 1 to n do
    scanf " %d" @@ Array.set a i
  done

let uf = Array.init (n+1)  ~f:Union_find.create
let es = Array.init 200005 ~f:(fun _ -> [])
let () =
  for _ = 1 to m do
    let u, v = scanf " %d %d" Tuple2.create in
    let u, v = if a.(u) < a.(v) then u, v else v, u in
    if u = v then
      Union_find.union uf.(u) uf.(v)
    else
      es.(a.(u)) <- (u, v) :: es.(a.(u));
  done

let dp = Array.create ~len:(n+1) (-1_000_000_000)
let () =
  dp.(Union_find.get uf.(1)) <- 1;
  Array.iter es ~f:(
    List.iter ~f:(fun (u, v) ->
    let u = Union_find.get uf.(u) in
    let v = Union_find.get uf.(v) in
    if 0 < dp.(u) then
      dp.(v) <- max dp.(v) (dp.(u) + 1)
    )
  )

let ans = max 0 dp.(Union_find.get uf.(n))
let () = printf "%d\n%!" ans
