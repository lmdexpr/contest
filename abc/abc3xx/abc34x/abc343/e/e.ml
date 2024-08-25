open Core
open Scanf

let v1, v2, v3 = scanf "%d %d %d" Tuple3.create

let f3 (a1, b1, c1) (a2, b2, c2) (a3, b3, c3) =
  let min a b c = min a @@ min b c in
  let max a b c = max a @@ max b c in
  (max 0 0 @@ min a1 a2 a3 + 7 - max a1 a2 a3) *
  (max 0 0 @@ min b1 b2 b3 + 7 - max b1 b2 b3) *
  (max 0 0 @@ min c1 c2 c3 + 7 - max c1 c2 c3)

let f2 (a1, b1, c1) (a2, b2, c2) =
  (max 0 @@ min a1 a2 + 7 - max a1 a2) *
  (max 0 @@ min b1 b2 + 7 - max b1 b2) *
  (max 0 @@ min c1 c2 + 7 - max c1 c2)

let () =
  let p1 = (0, 0, 0) in
  for a2 = -7 to 7 do for b2 = -7 to 7 do for c2 = -7 to 7 do
    let p2 = (a2, b2, c2) in
    for a3 = -7 to 7 do for b3 = -7 to 7 do for c3 = -7 to 7 do
      let p3 = (a3, b3, c3) in

      let nv3 = f3 p1 p2 p3 in
      let nv2 = f2 p1 p2 + f2 p1 p3 + f2 p2 p3 - nv3 * 3 in
      let nv1 = 3 * 7 * 7 * 7 - nv2 * 2 - nv3 * 3 in

      if v1 = nv1 && v2 = nv2 && v3 = nv3 then (
        printf "Yes\n0 0 0 %d %d %d %d %d %d\n" a2 b2 c2 a3 b3 c3;
        exit 0
      )
    done done done
  done done done;
  printf "No\n"
