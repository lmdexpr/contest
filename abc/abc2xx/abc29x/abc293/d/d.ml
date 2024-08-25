open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let x = Array.init m ~f:(fun _ -> scanf " %d %c %d %c" @@ fun a _ c _ -> a - 1, c - 1)

let r  = Array.init m ~f:(const 0)
let uf = Array.init n ~f:Union_find.create
let () = Array.iteri x ~f:(fun i (a, c) -> Union_find.union uf.(a) uf.(c); r.(i) <- a)

let v = Array.init n ~f:(const 0)
let e = Array.init n ~f:(const 0)
let () =
  for i = 0 to n - 1 do
    let i = Union_find.get uf.(i) in v.(i) <- v.(i) + 1
  done;
  for i = 0 to m - 1 do
    let i = Union_find.get uf.(r.(i)) in e.(i) <- e.(i) + 1
  done

let connected = Array.map uf ~f:Union_find.get |> Int.Set.of_array

let loops = Int.Set.count connected ~f:(fun i -> v.(i) = e.(i))

let () = printf "%d %d\n%!" loops (Int.Set.length connected - loops)
