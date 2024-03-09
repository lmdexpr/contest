open Core

let scan_2_values () = Scanf.scanf " %d %d" @@ fun x y -> x, y

let n, m = scan_2_values ()

module SI = Set.Make(Int)

let d = Array.create ~len:n 0
let acc = Array.create ~len:n @@ SI.empty

let () =
  for _ = 1 to m do
    let a, b = scan_2_values () in
    acc.(a-1) <- SI.add acc.(a-1) b;
    acc.(b-1) <- SI.add acc.(b-1) a;
    d.(a-1) <- d.(a-1) + 1;
    d.(b-1) <- d.(b-1) + 1;
  done;
  let open Out_channel in
  for i = 0 to n - 1 do
    printf "%d" d.(i);
    Set.iter acc.(i) ~f:(fun e -> printf " %d" e);
    newline stdout
  done
