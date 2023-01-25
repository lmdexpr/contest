open Core

let n = Scanf.scanf "%d" ident

let q = Scanf.scanf " %d" ident
let q = Array.init q ~f:(fun _ -> Scanf.scanf " %d %d %d %d" @@ fun t x y v -> t, x - 1, y - 1, v)
let[@inline] process ~case0 ~case1 = 
  Array.iter q ~f:(fun (t, x, y, v) ->
      match t with
      | 0 -> case0 x y v
      | _ -> case1 x y v
    )

let hints = Array.init (n-1) ~f:(const 0)
let () =
  process
    ~case0:(fun x _ v -> hints.(x) <- v)
    ~case1:(fun _ _ _ -> ())

let potential = Array.init n ~f:(const 0)
let () =
  for i = 0 to n - 2 do
    potential.(i + 1) <- hints.(i) - potential.(i)
  done

let uf = Array.init n ~f:Union_find.create
let () =
  process
    ~case0:(fun x y _ -> Union_find.union uf.(x) uf.(y))
    ~case1:(fun x y v ->
        if not @@ Union_find.same_class uf.(x) uf.(y) then printf "Ambiguous\n%!"
        else
          let sgn = if (x + y) % 2 = 0 then -1 else 1 in
          printf "%d\n%!" @@ potential.(y) + sgn * (potential.(x) - v)
      )
