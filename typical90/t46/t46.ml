open Core

let n = Scanf.scanf "%d" ident

let scan_mod_46 _ = Scanf.scanf " %d" @@ fun x -> x % 46

let a = Array.init 46 ~f:(const 0)
let () =
  for _ = 1 to n do
    let i = scan_mod_46 () in
    a.(i) <- a.(i) + 1
  done

let b = Array.init 46 ~f:(const 0)
let () =
  for _ = 1 to n do
    let i = scan_mod_46 () in
    b.(i) <- b.(i) + 1
  done

let c = Array.init 46 ~f:(const 0)
let () =
  for _ = 1 to n do
    let i = scan_mod_46 () in
    c.(i) <- c.(i) + 1
  done

let (let+) x k = Iter.flat_map k x
let (let^) x k = Iter.filter_map k x

let iter =
  let+ x = Iter.(0 -- 45) in
  let+ y = Iter.(0 -- 45) in
  let^ z = Iter.(0 -- 45) in
  Option.some_if ((x + y + z) % 46 = 0) @@ a.(x) * b.(y) * c.(z)

let () = printf "%d\n%!" @@ Iter.sum iter
