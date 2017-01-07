
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

let impr2 tab = 
  let size = Array.length tab in
  for i = 0 to (size-1) do
    let flight = tab.(i) in
    Printf.printf "%s : t_eff=%d t_rwy=%d\n" (getCallsign flight) (get_t_eff flight) (get_t_rwy flight) done;;

let cmp_tmin = fun v1 v2 ->
  compare (get_t_rwy v1) (get_t_rwy v2);;

  
let load () = read "data/lfpg_flights.txt";;
(* Charge la liste des vols du fichier _file (global) *)

let tableau = list_to_tab (load ());;
(*passe la liste des vols en tableau *)

Array.sort cmp_tmin tableau;;


let sequence = fun retard i tab ->
  let borne = ref max_int in
  let n =  Array.length tab in
  let solution = ref (Array.copy tab) in
  let rec sequence_rec = fun retard i ->
    if i < n
    then
      for j = i to (n-1) do
        if (i=j) || get_size tab.(i) != get_size tab.(j)
        then
	  if (i != j)
	  then
	    echanger tab i j;
	if (i = 0)
	then
          begin
	    tab.(i) <- (change_t_eff tab.(i) (get_t_rwy tab.(i)));
            (*Printf.printf "%s : teff = %d t_rwy = %d\n" (getCallsign tab.(i)) (get_t_eff tab.(i)) (get_t_rwy tab.(i));*)
          end
	else
	  begin
	    tab.(i) <- change_t_eff tab.(i) (max (get_t_rwy tab.(i)) (get_t_eff tab.(i-1) + (separation tab.(i-1) tab.(i))));
	    (*Printf.printf "%s : teff = %d t_rwy = %d\n" (getCallsign tab.(i)) (get_t_eff tab.(i)) (get_t_rwy tab.(i));*)
	  end;
	let retardi  = retard + get_t_eff tab.(i) - get_t_rwy tab.(i) in
	if retardi < !borne
	then
       	    sequence_rec retardi (i+1)
        else
          begin
	    if i != j
	    then
              begin
	        echanger tab i j;
                end
          end;
      done
    else
      begin
        borne := retard;
        solution := Array.copy tab;
      end;
  in sequence_rec retard i;
  !solution;;

let tableau_sub = Array.sub tableau 0 8;;
let solution = sequence 0 0 tableau_sub;;
impr2 solution;;



(*
let get_New_t_debut tab ->
  for i = 0 to Array.length tab
  do
    get_h_
*)

