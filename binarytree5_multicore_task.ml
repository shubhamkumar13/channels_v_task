module T = Domainslib.Task

let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let max_depth = try int_of_string Sys.argv.(2) with _ -> 10
let pool = T.setup_pool ~num_domains:(num_domains - 1)

type 'a tree = Empty | Node of 'a tree * 'a tree

let rec make d =
(* if d = 0 then Empty *)
  if d = 0 then Node(Empty, Empty)
  else let d = d - 1 in Node(make d, make d)

let rec check t =
  Domain.Sync.poll ();
  match t with
  | Empty -> 0
  | Node(l, r) -> 1 + check l + check r

let min_depth = 4
let max_depth = max (min_depth + 2) max_depth
let stretch_depth = max_depth + 1
(* let channels = Array.make (num_domains - 1) 0 *)

let () =
  (* Gc.set { (Gc.get()) with Gc.minor_heap_size = 1024 * 1024; max_overhead = -1; }; *)
  let _ = check (make stretch_depth) in
  ()
  (* Printf.printf "stretch tree of depth %i\t check: %i\n" stretch_depth c *)

let long_lived_tree = make max_depth

let values = Array.make num_domains 0
(* 
let calculate d st en ind =
  (* Printf.printf "st = %d en = %d\n" st en; *)
  let c = ref 0 in
  for _ = st to en do
  c := !c + check (make d)
  done;
  (* Printf.printf "ind = %d\n" ind; *)
  values.(ind) <- !c *)

let loop_depths d =
  for i = 0 to  ((max_depth - d) / 2 + 1) - 1 do
    let d = d + i * 2 in
    let niter = 1 lsl (max_depth - d + min_depth) in

    (* ///////////////////////////////////////////////
    	start_index, end_index and calculate are dependent
    	on the number of domains spawned.

    	but to pass all the information aside from `num_domains`
    	is very tricky in parallel_for.

    	since the chunking logic has to be 
    	an iteration of num_domains (as far as I can see now)

    	So I had to make calculate a local function.

    	it doesn't effect usage of calculate 
    	since only `loop_depths` function calls `calculate`
    	so this made `d` `st` `en` all available as local variables

    	this made it possible for me to use parallel_for 
      ////////////////////////////////////////////////
      *)
    let start_index index = (index * niter) / num_domains in
    let end_index index = (((index + 1) * niter) / num_domains) - 1 in
    let calculate index =
    	let c = ref 0 in
    	let st = start_index index in
    	let en = end_index index in
    	for _ = st to en do
    		c := !c + check (make d)
    	done;
    	values.(index) <- !c in
    T.parallel_for pool ~chunk_size:(num_domains) ~start:0 ~finish:(num_domains - 1) ~body:(fun index -> calculate index);

    let _ = Array.fold_left (+) 0 values in
    ()
    (* Printf.printf "%i\t trees of depth %i\t check: %i\n" niter d sum *)
  done

let () =
  loop_depths min_depth;
  let _ = max_depth in
  let _ = (check long_lived_tree) in
  T.teardown_pool pool