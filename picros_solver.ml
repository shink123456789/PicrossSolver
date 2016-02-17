type masutype =
    Unknown(* U *)
  | Black  (* K *)
  | White  (* W *)
  | Red    (* R *)
  | Green  (* G *)
  | Blue   (* B *)
  | Yellow (* Y *)
  | Mazenta(* M *)
  | Cian   (* C *)
  | Brown  (* O *)
  | Gray   (* A *)
;;

let string_of_masu = function
    Unknown -> "U"
  | Black   -> "K"
  | White   -> "W"
  | Red     -> "R"
  | Green   -> "G"
  | Blue    -> "B"
  | Yellow  -> "Y"
  | Mazenta -> "M"
  | Cian    -> "C"
  | Brown   -> "O"
  | Gray    -> "A"
;;

let col_of_string = function
(*  | "U" -> Unknown *)
  | "K" -> Black
(*  | "W" -> White *)
  | "R" -> Red
  | "G" -> Green
  | "B" -> Blue
  | "Y" -> Yellow
  | "M" -> Mazenta
  | "C" -> Cian
  | "O" -> Brown
  | "A" -> Gray
  | other -> failwith "not supported character."
;;

let print_masu m = print_string (string_of_masu m);;

let debug_print s =
  if false then print_endline s
;;

type linetype = Success of masutype list | Failure
;;

exception Unsolvable
;;

let rec merge_line l1 l2 =
  match (l1, l2) with
  | ([],[]) -> []
  | ((h1 :: t1), (h2 :: t2)) ->
      (if h1 = h2 then h1 else Unknown) :: merge_line t1 t2
  | other -> failwith "FATAL ERROR : merge_line -> line length is not matched."
;;

let rec ensure_all_white list =
  match list with
  | [] -> Success []
  | h :: t ->
      if h = White or h = Unknown then(
	match (ensure_all_white t) with
	| Failure -> Failure
	| Success el -> Success (White :: el)
       )
      else Failure
;;

let solve_line mslist_orig kylist_orig =
  let rec solve_line_sub mslist kylist fillednum =
    match mslist with
    | [] ->
	( match kylist with
      (* no key remains *)
	| [] -> Success []
	
      (* last one key *)
	| (kycol,kynum) :: [] ->
	    if kynum = fillednum then
	      Success []
	    else Failure

      (* keys remains *)
	| other -> Failure
	 )
    | nowms :: msrest ->
	( match kylist with

      (* no key remains -> rests are all white *)
	| [] -> ensure_all_white mslist

      (* now key *)
	| (kycol, kynum) :: kyrest ->

          (* this key start ? *)
            if fillednum = 0 then
              ( match nowms with
              | Unknown ->
                  let fil_res = solve_line_sub msrest kylist 1 in
                  let crs_res = solve_line_sub msrest kylist 0 in
                  ( match (fil_res, crs_res) with
                  | (Success fr, Success cr) ->
                      Success (Unknown :: merge_line fr cr)
                  | (Success fr, Failure) -> Success (kycol :: fr)
                  | (Failure, Success cr) -> Success (White :: cr)
                  | (Failure, Failure) -> Failure
                   )
              | White ->
                  ( match (solve_line_sub msrest kylist 0) with
                  | Success r -> Success (White ::r)
                  | Failure -> Failure
                   )
              | other ->
		  if kycol = other then 
                    ( match (solve_line_sub msrest kylist 1) with
                    | Success r -> Success (kycol ::r)
                    | Failure -> Failure
                     )
		  else Failure
               )

          (* this key was end *)
            else if fillednum = kynum then(

            (* next must be white or other color *)
	      if nowms = kycol then Failure
	      else if nowms = Unknown then(
		match kyrest with
		| [] -> ensure_all_white mslist
		| (kycol2,kynum2)::kyrest2 ->(
		    if kycol = kycol2 then
		      ( match solve_line_sub msrest kyrest 0 with
		      | Failure -> Failure
		      | Success r -> Success (White :: r)
		       )
		    else solve_line_sub mslist kyrest 0
		   )
	       )

            (* start next ! *)
	      else(
		solve_line_sub mslist kyrest 0
	       )
	     )

          (* this key continues *)
	    else(
	      if nowms = Unknown or nowms = kycol then
		( match solve_line_sub msrest kylist (fillednum + 1) with
		| Failure -> Failure
		| Success r -> Success (kycol :: r)
		 )
	      else Failure
	     )
	 )
  in
  match solve_line_sub mslist_orig kylist_orig 0 with
  | Failure ->
      raise Unsolvable
(* failwith ("This problem couldn't be solved.") *)
  | Success mslist_new ->(
      let rec dif_lists = function
	| ([], []) -> false
	| (h1::t1, h2::t2) ->
	    if h1 = h2 then dif_lists (t1, t2)
	    else true
	| other -> failwith "FATAL ERROR : solve_line - list length is changed!"
      in (mslist_new, dif_lists (mslist_orig, mslist_new))
     )
;;

let solve_line_h masus horkeylists =
  let newmasus = 
    Array.make_matrix (Array.length masus) (Array.length masus.(0)) Unknown in
  let rec solve_line_h_loop changed now max =
    (debug_print ("solve_line_h loop : line " ^ (string_of_int now) ^ "."));
    if now=max then changed
    else(
      let not_fixed = ref false in
      let oldlin = Array.make (Array.length masus) Unknown in
      Array.iteri
        (fun i masusi ->
	  if masusi.(now) = Unknown then not_fixed := true;
	  Array.set oldlin i masusi.(now)
	) masus;
      if !not_fixed then
	let (newlin_list, changedn) = solve_line (Array.to_list oldlin) horkeylists.(now) in
	let newlin = Array.of_list newlin_list in
	Array.iteri
          (fun i newlini -> Array.set newmasus.(i) now newlini) newlin;
	(solve_line_h_loop (changed or changedn) (now+1) max)
      else
	let newlin = oldlin in
	Array.iteri
          (fun i newlini -> Array.set newmasus.(i) now newlini) newlin;
	(solve_line_h_loop changed (now+1) max)
    )
  in
  let changed = solve_line_h_loop false 0 (Array.length horkeylists) in
    (newmasus, changed)
;;

let solve_line_v masus verkeylists =
  let newmasus = 
    Array.make_matrix (Array.length masus) (Array.length masus.(0)) Unknown in
  let rec solve_line_v_loop changed now max =
    (debug_print ("solve_line_v loop : line " ^ (string_of_int now) ^ "."));
    if now=max then changed
    else(
      let not_fixed = ref false in
      Array.iteri
	(fun i m ->
	  if m = Unknown then not_fixed := true;
	) masus.(now);
      if !not_fixed then
	let (newlin_list, changedn) = solve_line (Array.to_list masus.(now)) verkeylists.(now) in
	let newlin = Array.of_list newlin_list in
	Array.set newmasus now newlin;
	(solve_line_v_loop (changed or changedn) (now+1) max)
      else(
	Array.set newmasus now masus.(now);
	(solve_line_v_loop changed (now+1) max)
      )
    )
  in
  let changed = solve_line_v_loop false 0 (Array.length verkeylists) in
    (newmasus, changed)
;;


let solve_heuristics_once horkeys verkeys masus =
  let (masus2, changedh) = solve_line_h masus horkeys in
  debug_print "solve_line_h done.";
  let (masus3, changedv) = solve_line_v masus2 verkeys in
  debug_print "solve_line_v done.";
  (masus3, changedh or changedv)
;;

let rec solve_heuristics horkeys verkeys masus =
  debug_print "heuristic start.";
  let (masus_new, changed) = solve_heuristics_once horkeys verkeys masus in
  if changed then(
    (debug_print "changed.");
    solve_heuristics horkeys verkeys masus_new
   )
  else(
    debug_print "heuristic end.";
    masus_new
   )
;;

let solve_pre_sub kylist mssize =
  let rec sum_ky = function
    | [] -> 0
    | (kycol, kynum) :: [] -> kynum
    | (kycol, kynum) :: (kycol2, kynum2) :: rest ->
	kynum +
	  (if kycol = kycol2 then 1
	  else 0) +
	  (sum_ky ((kycol2, kynum2) :: rest))
  in
  let fuz = mssize - (sum_ky kylist) in
  let mss = Array.make mssize Unknown in
  (debug_print ("fuz : " ^ (string_of_int fuz)));
  let rec fill_musts_sub now next col=
    if now = next then ()
    else(
      (debug_print ("now : " ^ (string_of_int now) ^ " next : " ^ (string_of_int next)));
      mss.(now) <- col;
      fill_musts_sub (now+1) next col
     )
  in
  let rec fill_musts now = function
    | [] -> ()
    | (kycol, kynum) :: [] ->(
	fill_musts_sub
	  (now + (if fuz > kynum then kynum else fuz))
	  (now + kynum) kycol
       )
    | (kycol, kynum) :: (kycol2, kynum2) :: kyrest ->(
	fill_musts_sub
	  (now + (if fuz > kynum then kynum else fuz))
	  (now + kynum) kycol;
	fill_musts
	  (now + kynum +
	     (if kycol = kycol2 then 1 else 0)
	  )
	  ((kycol2, kynum2) :: kyrest)
       )
  in fill_musts 0 kylist;
  mss
;;

let solve_pre horkeys verkeys =
  (debug_print "pre start.");
  let h = Array.length horkeys in
  let w = Array.length verkeys in
  let masus = Array.make_matrix w h Unknown in
  Array.iteri
    (fun y hky ->
      (debug_print ("pre sub start : " ^ (string_of_int y)));
      let res = solve_pre_sub hky w in
      (debug_print ("pre sub end : " ^ (string_of_int y)));
      Array.iteri
	(fun x resi ->
	  masus.(x).(y) <- resi
	) res
    ) horkeys;
  (debug_print "pre h end.");
  Array.iteri
    (fun x vky ->
      let res = solve_pre_sub vky h in
      Array.iteri
	(fun y resi ->
	  masus.(x).(y) <- resi
	) res
    ) verkeys;
  (debug_print "pre v end.");
  masus
;;

let solve horkeys verkeys =
(*
  let h = Array.length horkeys in
  let w = Array.length verkeys in
*)
  let masus = solve_pre horkeys verkeys in
  solve_heuristics horkeys verkeys masus
;;

let read_keylist line =
  let rec read_keylist_loop subline =
    try(
      let ci = String.index subline ',' in
      let ky = String.sub subline 0 ci in
      ( if ky.[0] >= '0' && ky.[0] <= '9' then
	(Black, int_of_string ky)
      else 
	let kyc = col_of_string (String.sub subline 0 1) in
	let kyn = int_of_string (String.sub subline 1 (ci-1)) in
	(kyc,kyn)
       )::read_keylist_loop
	    (String.sub subline (ci+1) ((String.length subline)-ci-1))
     ) with Not_found -> []
  in read_keylist_loop line
;;

let read_keylistlist keynum inch =
  let rec read_keylistlist_loop now =
    if now = keynum then []
    else(
      let line = input_line inch in
      ((read_keylist line )
      :: (read_keylistlist_loop (now+1)))
     )
  in read_keylistlist_loop 0
;;

let read_keys inch =
  let verkeynum = int_of_string (input_line inch) in
  let horkeynum = int_of_string (input_line inch) in
  let verkeys = Array.of_list (read_keylistlist verkeynum inch) in
  let horkeys = Array.of_list (read_keylistlist horkeynum inch) in
  (verkeys, horkeys)
;;

let print_masus masus =
  Array.iteri (fun y my ->
    Array.iteri (fun x mx ->
      print_masu masus.(x).(y);
      print_string ",";
    ) masus;
    print_newline();
  ) masus.(0)
;;

let string_of_ky (kycol, kynum) =
  (string_of_masu kycol) ^ (string_of_int kynum)
;;

let rec string_of_kylist = function
  | [] -> ""
  | ky :: kyrest ->
      (string_of_ky ky) ^ " " ^ (string_of_kylist kyrest)
;;

let string_of_keys verkeys horkeys =
  let ret = ref "" in
  Array.iteri (fun i vklist ->
    ret := !ret ^ (string_of_int i) ^ ":" ^ (string_of_kylist vklist) ^ "\n"
	      ) verkeys;
  Array.iteri (fun i hklist ->
    ret := !ret ^ (string_of_int i) ^ ":" ^ (string_of_kylist hklist) ^ "\n"
	      ) horkeys;
  !ret
;;

let _ =
  let (verkeys, horkeys) = read_keys stdin in
  (debug_print "data is read.");
  (debug_print (string_of_keys verkeys horkeys));
  print_masus (solve horkeys verkeys)
;;
