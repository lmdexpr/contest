open Core

let n = Scanf.scanf "%d" ident

let p = Array.init n ~f:(fun _ -> Scanf.scanf " %d %d" Tuple2.create)

let arg (ox, oy) (x, y) = Float.atan2 (float @@ y - oy) (float @@ x - ox) *. 180.0 /. Float.pi

let angle2 t1 t2 =
  let angle = Float.abs @@ t1 -. t2 in
  if Float.(angle > 180.0) then 360.0 -. angle else angle

let () =
  Iter.of_array p
  |> Iter.mapi (fun i b ->
      let m = n - 1 in
      let iter =
        Iter.of_array p
        |> Iter.filter_mapi (fun j e -> Option.some_if (j <> i) (arg b e))
        |> Iter.sort ~cmp:compare_float
      in
      let sorted_args = Iter.to_array iter in
      let f max t1 =
        let target = t1 +. 180.0 in
        let target = target -. if Float.(t1 < 360.0) then 0.0 else 360.0 in
        let k =
          Array.binary_search sorted_args ~compare:compare_float `First_strictly_greater_than target
          |> Option.value ~default:(m-1)
        in
        let a1 = angle2 t1 sorted_args.(k % m) in
        let a2 = angle2 t1 sorted_args.((k - 1) % m) in
        Float.max max @@ Float.max a1 a2
      in
      Iter.fold f 0.0 iter
    )
  |> Iter.max_exn ~lt:Float.(<)
  |> printf "%f\n%!"
