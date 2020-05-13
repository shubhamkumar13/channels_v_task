module A = Array
module L = List
module C = Domainslib.Chan
let num_domains = try int_of_string Sys.argv.(1) with _ -> 4
let input_fn = try Sys.argv.(2) with _ ->  "data/tox21_nrar_ligands_std_rand_01.csv"
(* let chunk_size = try int_of_string Sys.argv.(3) with _ -> 16 *)
let dot_product xs ys =
  let n1 = A.length xs in
  assert(n1 = A.length ys);
  let res = ref 0.0 in
  for i = 0 to n1 - 1 do
    res := !res +. ((A.unsafe_get xs i) *. (A.unsafe_get ys i))
  done;
  !res


let parse_line line =
  let int_strings = Utls.string_split_on_char ' ' line in
  let nb_features = L.length int_strings in
  let res = A.create_float nb_features in
  L.iteri (fun i int_str ->
      A.unsafe_set res i (float_of_string int_str)
    ) int_strings;
  res

type message = Do of (unit -> unit) | Quit

type chan = {req: message C.t; resp: unit C.t}
let channels =
  Array.init (num_domains - 1) (fun _ -> {req= C.make_bounded 1; 
  resp= C.make_bounded 1})  

let rec worker c () =
  match C.recv c.req with
  | Do f ->
      f () ; C.send c.resp () ; worker c ()
  | Quit ->
      ()

let samples = A.of_list (Utls.map_on_lines_of_file input_fn parse_line)
let n = (A.length samples) / 2 + ((A.length samples) mod 2)
let q =  (A.length samples)
let res = A.init q (fun _ -> A.create_float q)

let distribution =
  let rec loop n d acc =
    if d = 1 then n::acc
    else
      let w = n / d in
      loop (n - w) (d - 1) (w::acc)
  in
  let l = loop n num_domains [] in
  Array.of_list l


let run_iter job =
  let sum = ref 0 in
  Array.iteri (fun i c ->
    let begin_ = !sum in
    let end_ = begin_ + distribution.(i) in
    sum := end_;
    C.send c.req (Do (job begin_ end_));) channels;
  job !sum (!sum + distribution.(num_domains - 1)) ();
  Array.iter (fun c -> C.recv c.resp) channels

let compute_gram_matrix samples res s e =
  let p = (A.length samples)-1 in
  assert(p > 0);
  for i = s to e do
    for j = 0 to p  do
      if i = j then
        let x = dot_product samples.(i) samples.(j) 
        and sc = dot_product samples.(p-i) samples.(p-j)in
        res.(i).(i) <- x;
        res.(p-i).(p-j) <- sc 
      else
        if i < j then
          let x = dot_product samples.(i) samples.(j) in
          res.(i).(j) <- x;
          res.(j).(i) <- x (* symmetric matrix *)
      else 
        if i > j then  
        let e = p-i and f = p-j in
        let x = dot_product samples.(e) samples.(f) in
        res.(e).(f) <- x;
        res.(f).(e) <- x (* symmetric matrix *)
    done
  done
  
let aux () =
  run_iter (fun s e () -> compute_gram_matrix samples res s e ) 

let _ =
  let domains = Array.map (fun c -> Domain.spawn (worker c)) channels in
  Printf.printf "samples: %d features: %d\n"
      (A.length samples) (A.length samples.(0));
  aux() ;
  Array.iter (fun c -> C.send c.req Quit) channels;
  Array.iter Domain.join domains;
  Utls.print_matrix res