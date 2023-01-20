open Core

let n = Scanf.scanf "%d" ident

let b = Array.init n ~f:(const 0)
let () =
  for i = 1 to n - 1 do
    Scanf.scanf " %d" @@ fun bi -> b.(i) <- bi - 1
  done

let mins = Array.init n ~f:(const 1000_000_000)
let maxs = Array.init n ~f:(const @@ -1)

let salary = Array.init n ~f:(const 0)

let () =
  for i = n - 1 downto 0 do
    salary.(i) <-
      if maxs.(i) = -1 then 1
      else
        maxs.(i) + mins.(i) + 1;
    maxs.(b.(i)) <- max maxs.(b.(i)) salary.(i);
    mins.(b.(i)) <- min mins.(b.(i)) salary.(i)
  done

let () = printf "%d\n%!" salary.(0)
