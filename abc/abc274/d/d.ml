open Core
open Scanf

let n, x, y = scanf "%d %d %d" Tuple3.create

let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let zero = 10000

let dp_1dim a dp =
  let buf = Array.init 20001 ~f:(const false) in
  Iter.(0 -- (20000 - a)) |> Iter.iter (fun j ->
      buf.(j + a) <- buf.(j + a) || dp.(j);
      buf.(j) <- buf.(j) || dp.(j + a)
    );
  buf

let dp1 = Array.init 20001 ~f:(const false)
and dp2 = Array.init 20001 ~f:(const false)
let () =
  dp1.(zero + a.(0)) <- true; dp2.(zero) <- true

let dp1, dp2 =
  Iter.(1 -- (n - 1)) |> Fn.flip Iter.fold (dp1, dp2)
    (fun (dp1, dp2) i ->
       if i % 2 = 0
       then dp_1dim a.(i) dp1, dp2
       else dp1, dp_1dim a.(i) dp2
    )

let () =
  printf @@ if dp1.(x + zero) && dp2.(y + zero) then "Yes\n%!" else "No\n%!"
