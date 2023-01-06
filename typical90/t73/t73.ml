open Core

let n = Scanf.scanf "%d" ident
let is_a = Array.init n ~f:(fun _ -> Scanf.scanf " %c" Char.((=) 'a'))

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v

  let around g v = find g v |> Option.value ~default:Iter.empty
end

let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to n - 1 do
    Scanf.scanf " %d %d" (fun u v -> Graph.connect g (u - 1) (v - 1))
  done

let modulo = 1000000007
let (+%) a b = (a + b) % modulo
and (-%) a b = (a - b + modulo) % modulo
and( *%) a b = a * b % modulo

let dp = Array.init n ~f:(fun _ -> Array.init 3 ~f:(const 0))
let rec dfs pred now =
  let only, both =
    Graph.around g now
    |> Iter.filter ((<>) pred)
    |> Iter.fold
      (fun (only, both) u ->
         dfs now u;
         let dp = dp.(u) in
         if is_a.(now) then
           only *% (dp.(1) + dp.(2)),
           both *% (dp.(0) +% dp.(1) +% 2 * dp.(2))
         else
           only *% (dp.(0) + dp.(2)),
           both *% (dp.(0) +% dp.(1) +% 2 * dp.(2))
      )
      (1, 1)
  in
  let dp = dp.(now) and i  = Bool.to_int is_a.(now) in
  dp.(i) <- only;
  dp.(2) <- both -% only

let () =
  dfs (-1) 0;
  printf "%d\n%!" dp.(0).(2)
