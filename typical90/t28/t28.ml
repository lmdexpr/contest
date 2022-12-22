open Core
    
let n = Scanf.scanf "%d" ident

let imos = Array.make_matrix ~dimx:1001 ~dimy:1001 0
let () =
  for _ = 1 to n do
    let scan_pair () = Scanf.scanf " %d %d" Tuple2.create in
    let lx, ly = scan_pair ()
    and rx, ry = scan_pair () in
    imos.(lx).(ly) <- imos.(lx).(ly) + 1;
    imos.(rx).(ry) <- imos.(rx).(ry) + 1;
    imos.(lx).(ry) <- imos.(lx).(ry) - 1;
    imos.(rx).(ly) <- imos.(rx).(ly) - 1;
  done;
  for i = 0 to 1000 do
    for j = 1 to 1000 do
      imos.(i).(j) <- imos.(i).(j - 1) + imos.(i).(j)
    done
  done;
  for j = 0 to 1000 do
    for i = 1 to 1000 do
      imos.(i).(j) <- imos.(i - 1).(j) + imos.(i).(j)
    done
  done

let ans = Array.init (n + 1) ~f:(const 0)
let () =
  for i = 0 to 1000 do
    for j = 0 to 1000 do
      let k = imos.(i).(j) in
      ans.(k) <- ans.(k) + 1
    done
  done

let () =
  for i = 1 to n do
    printf "%d\n" ans.(i)
  done
