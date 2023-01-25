open Core

let n, m = Scanf.scanf "%d %d" Tuple2.create

let scan_1 _ = Scanf.scanf " %d" ident

let a = Array.init (n + 1) ~f:scan_1
let c = Array.init (n + m + 1) ~f:scan_1

let (/) ?(n=n) ?(m=m) l r =
  let ans = Array.init (m + 1) ~f:(const @@ -1) in
  for i = m downto 0 do
    ans.(i) <- l.(i + n) / r.(n);
    for j = n downto 0 do
      l.(i + j) <- l.(i + j) - ans.(i) * r.(j)
    done
  done;
  ans

let () =
  Array.iter (c / a) ~f:(printf "%d ");
  printf "\n%!"
