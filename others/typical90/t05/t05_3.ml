open Core

let modpow a b m =
  let open Int64 in
  Array.init 64 ~f:ident
  |> Array.fold ~init:(1L, a) ~f:(fun (p, q) i ->
      if b land (1L lsl i) <> 0L then p * q % m, q * q % m
      else
        p, q * q % m
    )
  |> Tuple2.get1
  |> to_int_exn

let modulo = 1000000007

let n, b, k = Scanf.scanf "%Ld %d %d" Tuple3.create
let c = Array.init k ~f:(fun _ -> Scanf.scanf " %d" ident)

let p10 = Array.init 64 ~f:Int64.(fun i -> modpow 10L (1L lsl i) (of_int b))

let dp = Array.make_matrix ~dimx:64 ~dimy:b 0
let answer = Array.make_matrix ~dimx:64 ~dimy:b 0
let () =
  Array.iter c ~f:(fun c -> dp.(0).(c % b) <- dp.(0).(c % b) + 1);
  answer.(0).(0) <- 1;
  for i = 0 to 62 do
    let p = Int64.(n land (1L lsl i) <> 0L) in

    for j = 0 to b - 1 do
      for k = 0 to b - 1 do
        let next = (j * p10.(i) + k) % b in
        dp.(i+1).(next) <- (dp.(i+1).(next) + dp.(i).(j) * dp.(i).(k)) % modulo;

        if p then
          answer.(i+1).(next) <- (answer.(i+1).(next) + answer.(i).(j) * dp.(i).(k)) % modulo
        else
          answer.(i+1).(j) <- answer.(i).(j)
      done
    done
  done

let () = printf "%d\n%!" answer.(63).(0)
