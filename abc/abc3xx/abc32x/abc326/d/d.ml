open Core
open Scanf

module Array = struct
  include Array
  let rec reverse a ~start ~stop =
    if start < stop then begin
      Array.swap a start stop;
      reverse a ~start:(start + 1) ~stop:(stop - 1)
    end
end

module Permutation = struct
  let next a ~l ~r =
    let downto_loop ~start ~stop ~p ~proc =
      Iter.(start --^ stop) |> Fn.flip Iter.fold_while false
      @@ fun _ i -> if p i then (proc i; true, `Stop) else false, `Continue
    in
    let change_to_next_permutation a ~l ~r =
      downto_loop ~start:(r - 1) ~stop:l
        ~p:(fun i -> a.(l) < a.(i))
        ~proc:(fun i ->
            Array.swap a l i;
            Array.reverse a ~start:(l + 1) ~stop:(r - 1)
          )
    in
    downto_loop ~start:(r - 2) ~stop:l
      ~p:(fun i -> a.(i) < a.(i + 1) && change_to_next_permutation a ~l:i ~r)
      ~proc:ignore

  let fold n ~f ~acc =
    let nums = Array.init n ~f:Fn.id in
    let rec permutations acc =
      let acc = f acc nums in
      let found_next = next nums ~l:0 ~r:n in
      if found_next then permutations acc else acc
    in
  permutations acc

  let iter n ~f = fold n ~f:(fun () -> f) ~acc:()
end

let n   = scanf "%d"  Fn.id
let row = scanf " %s" Fn.id
let col = scanf " %s" Fn.id

let rule1 a b c =
  Iter.(0 -- (n - 1)) |> Iter.map (fun i -> a.(i), b.(i), c.(i))
  |> Iter.for_all (fun (a, b, c) -> a <> b && b <> c && c <> a)

let create_from a b c =
  let ans = Array.make_matrix ~dimx:n ~dimy:n '.' in
  let set x = Array.iteri ~f:(fun i j -> ans.(i).(j) <- x) in
  set 'A' a; set 'B' b; set 'C' c;
  ans

let head f =
  Iter.(0 -- (n - 1))
  |> Iter.find_pred Char.(fun k -> f k <> '.')
  |> Option.value_map ~default:'*' ~f

let rule2 ans = String.(init n ~f:(fun i -> head @@ fun j -> ans.(i).(j)) = row)
and rule3 ans = String.(init n ~f:(fun j -> head @@ fun i -> ans.(i).(j)) = col)

let solve a b c =
  if rule1 a b c then
    let ans = create_from a b c in
    if rule2 ans && rule3 ans then (
      printf "Yes\n";
      Array.iter ans ~f:(fun ans -> Array.iter ans ~f:(printf "%c"); printf "\n");
      exit 0
    )

let () =
  Permutation.iter n ~f:(fun a ->
  Permutation.iter n ~f:(fun b ->
  Permutation.iter n ~f:(fun c -> solve a b c)));
  printf "No\n"
