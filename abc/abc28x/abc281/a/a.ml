open Core

let n = Scanf.scanf "%d" ident

let () =
  for i = n downto 0 do 
    printf "%d\n%!" i
  done
