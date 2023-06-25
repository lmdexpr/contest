open Core
open Scanf

let n = scanf "%d" ident

let s = Array.init n ~f:(fun _ -> scanf " %s" ident)

let () =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let s = s.(i) ^ s.(j) in
      if i <> j && String.(equal s (rev s)) then begin
        printf "Yes\n%!";
        exit 0
      end
    done
  done

let () = printf "No\n%!"
