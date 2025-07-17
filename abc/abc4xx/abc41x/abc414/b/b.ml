open Core
open Scanf

let n = scanf " %d" Fn.id

let s = Array.init n ~f:(fun _ -> scanf " %c %Ld" @@ fun c l ->
  c, Int64.(to_int_exn @@ min l 101L)
)

let () =
  if 100 < Array.sum (module Int) s ~f:snd then (
    printf "Too Long\n%!";
    exit 0
  )

let () =
  for i = 0 to pred n do
    for _ = 1 to snd s.(i) do
      printf "%c" @@ fst s.(i)
    done
  done;
  printf "\n%!";
