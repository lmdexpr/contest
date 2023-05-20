open Core

let h, m = Scanf.scanf "%d %d" @@ fun h m -> h, m

let split d = d / 10, d % 10

let misjudge h m =
  let a, b = split h
  and c, d = split m in
  let h, m = a * 10 + c, b * 10 + d in
  h < 24 && m < 60

let rec s h m =
  if misjudge h m then h, m
  else
    let m = (m + 1) % 60 in
    let h = if m = 0 then (h + 1) % 24 else h in
    s h m

let h, m = s h m

let () = printf "%d %d\n" h m
