open Core
open Scanf

let h, w, q = scanf "%d %d %d" Tuple3.create

let gh = Array.init h ~f:(fun _ -> Int.Set.empty)
let gw = Array.init w ~f:(fun _ -> Int.Set.empty)
let () =
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      gh.(i) <- Set.add gh.(i) j;
      gw.(j) <- Set.add gw.(j) i;
    done
  done

let lower_bound_h, lower_bound_w =
  let lower_bound g x =
    Set.binary_search g ~compare `First_greater_than_or_equal_to x
  in
  (fun i x -> lower_bound gh.(i) x |> Option.value ~default:h),
  (fun i x -> lower_bound gw.(i) x |> Option.value ~default:w)

let erase i j =
  gh.(i) <- Set.remove gh.(i) j;
  gw.(j) <- Set.remove gw.(j) i

let _ =
  for _ = 1 to q do
    let r, c = scanf " %d %d" Tuple2.create in
    let r, c = r - 1, c - 1 in

    if Set.mem gh.(r) c then erase r c
    else (
      let it = lower_bound_w c r in
      if it <> 0 then erase (it - 1) c;

      let it = lower_bound_w c r in
      if it <> w then erase it c;

      let it = lower_bound_h r c in
      if it <> 0 then erase r (it - 1);

      let it = lower_bound_h r c in
      if it <> h then erase r it
    )
  done

let ans = Array.sum (module Int) gh ~f:Set.length

let () = printf "%d\n%!" ans
