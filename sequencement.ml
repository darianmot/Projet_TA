
open Traffic;;

(****************************************************************************)

let echanger = fun tab i j -> 
	let memoire = tab.(i) in 
	tab.(i) <- tab.(j);
	tab.(j) <- memoire;;
		

let list_to_tab = fun list ->
  Array.of_list list;;

let impr = fun tab f ->
  let size = Array.length tab in
  for i = 0 to (size-1) do
    let flight = tab.(i) in
    Printf.printf "%d\n" (f flight) done;;

let cmp_tmin = fun v1 v2 ->
  compare (get_t_rwy v1) (get_t_rwy v2);;

  
let load () = read "data/lfpg_flights.txt";;
(* Charge la liste des vols du fichier _file (global) *)

let tableau = list_to_tab (load ());;
(*passe la liste des vols en tableau *)

Array.sort cmp_tmin tableau;;

(*impr tableau get_t_rwy;;*)


let same_cat = fun i j ->
   ((i,j) = (L,L) || (i,j) = (M,M) || (i,j) = (H,H));;

let sequence = fun retard i tab ->
  let borne = ref max_int in
  let n =  Array.length tab in
  let rec sequence_rec = fun retard i ->
    if i < n
    then
    for j = i to (n-1) do
      if (i=j) || (not (same_cat (get_size tab.(i)) (get_size tab.(j))))
      then
	if (i != j)
	then
	    echanger tab i j;
	if (i = 0)
	then
	  tab.(i) <- (change_t_eff tab.(i) (get_t_rwy tab.(i)))
	else
	  begin
	  tab.(i) <- change_t_eff tab.(i) (max (get_t_rwy tab.(i)) (get_t_eff tab.(i-1) + (separation tab.(i-1) tab.(i))));
	    Printf.printf "teff : %d\n" (get_t_eff tab.(i));
	  end;
	let retardi  = retard + (get_t_eff tab.(i)) - get_t_rwy tab.(i) in
	Printf.printf "retardi : %d t_rwy : %d\n" retardi (get_t_rwy tab.(i));
	if retardi < !borne
	then
       	  sequence_rec retardi (i+1);
	if i != j
	then
	   echanger tab i j
    done
    else 
      borne := retard;
    Printf.printf "retard : %d\n" !borne;
  in
  sequence_rec retard i ;;

let tableau_sub = Array.sub tableau 0 4;;

sequence 0 0 tableau_sub;;

impr tableau_sub get_t_eff;;

(*
let get_New_t_debut tab ->
  for i = 0 to Array.length tab
  do
    get_h_
*)

