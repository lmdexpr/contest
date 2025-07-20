open Core
open Scanf

let h = scanf " %d" Fn.id
let w = scanf " %d" Fn.id

let a = Array.init h ~f:(fun _ -> Array.init w ~f:(fun _ -> scanf " %Ld" Fn.id))

let p = Array.init (h+w-1) ~f:(fun _ -> scanf " %Ld" Fn.id)

let dp = Array.make_matrix ~dimx:h ~dimy:w (Int64.of_float 1e18)

let ch f i j v = dp.(i).(j) <- f dp.(i).(j) v

let () =
  dp.(h-1).(w-1) <- 0L;
  for i = h - 1 downto 0 do
    for j = w - 1 downto 0 do
      if i + 1 < h then ch Int64.min i j dp.(i + 1).(j);
      if j + 1 < w then ch Int64.min i j dp.(i).(j + 1);

      ch Int64.(+) i j @@ Int64.(-) p.(i + j) a.(i).(j);
      ch Int64.max i j 0L;
    done
  done

let () = printf "%Ld\n%!" dp.(0).(0)
