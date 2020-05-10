let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let mat_size = try int_of_string Sys.argv.(2) with _ -> 1200

module T = Domainslib.Task

module SquareMatrix = struct
  let create f : float array =
    let fa = Array.create_float (mat_size * mat_size) in
    for i = 0 to mat_size * mat_size - 1 do
      fa.(i) <- f (i / mat_size) (i mod mat_size)
    done;
    fa

  let get (m : float array) r c = m.(r * mat_size + c)
  let set (m : float array) r c v = m.(r * mat_size + c) <- v
  let copy = Array.copy
end

open SquareMatrix

let chunk_size = 32
let pool = T.setup_pool ~num_domains:(num_domains - 1)

let distribution size =
  let rec loop n d acc =
    if d = 1 then n::acc
    else if n < chunk_size then n::acc
    else if n/d < chunk_size then loop (n - chunk_size) (d-1) (chunk_size::acc)
    else
      let w = n / d in
      loop (n - w) (d - 1) (w::acc)
  in
  List.rev (loop size num_domains []) 


let lup a0 =
  let a = copy a0 in
  let aux k s e =
    (* Printf.printf "[%d] aux k=%d s=%d e=%d\n%!" d k s e; *)
    for row = s to (pred e) do
      let factor = get a row k /. get a k k in
      for col = k + 1 to mat_size-1 do
          set a row col (get a row col -. factor *. (get a k col))
      done;
      set a row k factor
    done
  in
  for k = 0 to (mat_size - 2) do
    let mine, rest =
      match distribution (mat_size - 2 - k) with
      | [] -> failwith "impossible"
      | x::xs -> x,xs
    in
    (* Printf.printf "\nmine=%d " mine; *)
    (* List.iter (fun r -> Printf.printf "%d " r) rest; *)
    let sum = ref (k + mine) in
    let pr_list =
      List.map (fun size ->
        let begin_ = !sum in
        sum := !sum + size;
        let end_ = !sum in
        T.async pool (fun _ -> aux k begin_ end_)) rest
    in
    aux k k (mine + k);
    List.iter (fun pr -> T.await pool pr) pr_list;
  done;
  a

let () =
  let a = create (fun _ _ -> (Random.float 100.0)+.1.0) in
  let lu = lup a in
  let _l = create (fun i j -> if i > j then get lu i j else if i = j then 1.0 else 0.0) in
  let _u = create (fun i j -> if i <= j then get lu i j else 0.0) in
  T.teardown_pool pool
