open Core

let n, p, q, r, s = Scanf.scanf "%d %d %d %d %d" @@ fun n p q r s -> n, p - 1, q - 1, r - 1, s - 1

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let () =
  for i = 0 to p - 1 do
    printf "%d " a.(i)
  done;
  for i = r to s do
    printf "%d " a.(i)
  done;
  for i = q + 1 to r - 1 do
    printf "%d " a.(i)
  done;
  for i = p to q do
    printf "%d " a.(i)
  done;
  for i = s + 1 to n - 1 do
    printf "%d " a.(i)
  done;
  printf "\n%!"
