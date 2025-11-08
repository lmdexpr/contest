open Core
open Scanf

module Imos2dMake (M : sig include Int_intf.S end) = struct
  type t = M.t array array

  let init h w : t = Array.make_matrix ~dimx:(h+2) ~dimy:(w+2) M.zero

  let update (t:t) ((a, b), (c, d)) =
    let c, d = c + 1, d + 1 in
    t.(a).(b) <- M.succ t.(a).(b);
    t.(a).(d) <- M.pred t.(a).(d);
    t.(c).(b) <- M.pred t.(c).(b);
    t.(c).(d) <- M.succ t.(c).(d)

  let finish imos : t = 
    let cumsum ~init ~f a =
      let paired f a b = let r = f a b in r, r in
      Array.folding_map a ~init ~f:(paired f)
    in
    let cumsum2d ~init ~f a =
      let n = Array.length a and m = Array.length a.(0) in
      let a = Array.map a ~f:(cumsum ~init ~f) in
      for i = 1 to n - 1 do
        for j = 0 to m - 1 do
          a.(i).(j) <- f a.(i).(j) a.(i-1).(j)
        done
      done;
      a
    in
    cumsum2d ~init:M.zero ~f:M.(+) imos

  let create h w xs : t =
    let t = init h w in
    List.iter xs ~f:(update t);
    finish t

  let _area t a b c d =
    let open M in
    t.(c).(d) - (t.(a).(d) + t.(c).(b)) + t.(a).(b)
end
module Imos2d = Imos2dMake(Int)

let h = scanf " %d" Fn.id
let w = scanf " %d" Fn.id
let n = scanf " %d" Fn.id

let imos = Imos2d.create h w @@
  List.init n ~f:(fun _ ->
    scanf " %d %d %d %d" @@ fun a b c d -> (a, b), (c, d)
  )

let () =
  for i = 1 to h do
    for j = 1 to w do
      printf "%d " imos.(i).(j) 
    done;
    printf "\n";
  done
