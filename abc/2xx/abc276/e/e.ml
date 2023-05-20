open Core

let h, w = Scanf.scanf "%d %d" @@ fun a b -> a, b

let sx = ref 0
let sy = ref 0

let c = Array.init h ~f:(fun i ->
    let s = Scanf.scanf " %s" ident in
    for j = 0 to w - 1 do
      if Char.equal s.[j] 'S' then begin
        sx := i;
        sy := j
      end
    done;
    s
  )

let is_valid (x, y) = 0 <= x && x < h && 0 <= y && y < w

let around = List.filter ~f:is_valid [
  !sx + 1, !sy;
  !sx - 1, !sy;
  !sx, !sy + 1;
  !sx, !sy - 1;
]

let uf = Array.init h ~f:(fun i -> Array.init w ~f:(fun j -> Union_find.create (i, j)))

let () =
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if Char.equal c.(i).[j] '.' then begin
        if i <> h - 1 && Char.equal c.(i+1).[j] '.' then
          Union_find.union uf.(i).(j) uf.(i+1).(j);
        if j <> w - 1 && Char.equal c.(i).[j+1] '.' then
          Union_find.union uf.(i).(j) uf.(i).(j+1)
      end
    done
  done;

  List.iter around ~f:(fun (sx, sy) ->
      List.iter around ~f:(fun (ex, ey) ->
          if (sx <> ex || sy <> ey) && Union_find.same_class uf.(sx).(sy) uf.(ex).(ey) then begin
            print_endline "Yes";
            exit 0
          end
        )
    );

  print_endline "No"
