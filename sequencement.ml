
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
    Printf.printf "%s\n" (f flight) done;;

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

let list_runway = fun tab ->
  let acc26L = ref [] in
  let acc26R = ref [] in
  let acc27L = ref [] in
  let acc27R = ref [] in
  for i=0 to ((Array.length tab)-1) do
    match (get_runway tab.(i)) with
	"26L" -> acc26L := tab.(i)::(!acc26L)
      |"26R" -> acc26R := tab.(i)::(!acc26R)
      |"27L" -> acc27L := tab.(i)::(!acc27L)
      |"27R" -> acc27R := tab.(i)::(!acc27R)
      |_ -> ();
  done;
  let res = [| !acc26L; !acc26R; !acc27L; !acc27R |] in
  res;;

let tri = list_runway tableau;;
let rwy26L = Array.sort cmp_tmin (list_to_tab tri.(0));;
let rwy26R = Array.sort cmp_tmin (list_to_tab tri.(1));;
let rwy27L = Array.sort cmp_tmin (list_to_tab tri.(2));;
let rwy27R = Array.sort cmp_tmin (list_to_tab tri.(3));;


let sequence_opti = fun retard i tab ->
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
          end
	else
	  begin
	    tab.(i) <- change_t_eff tab.(i) (max (get_t_rwy tab.(i)) (get_t_eff tab.(i-1) + (separation tab.(i-1) tab.(i))));
	  end;
	let retardi  = retard + get_t_eff tab.(i) - get_t_rwy tab.(i) in
	if retardi < !borne
	then
       	    sequence_rec retardi (i+1);
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
let solution = sequence_opti 0 0 tableau_sub;;
impr2 solution;;

let sequence_fifo = fun tab ->
  for i = 0 to Array.length tab -1 do
    if i = 0 then 
      tab.(i) <- change_t_eff tab.(i) (get_t_rwy tab.(i));
    else 
      tab.(i) <- change_t_eff tab.(i) (max (get_t_rwy tab(i)) (get_t_rwy tab.(i-1) + separation tab.(i-1) tab.(i))
   done;
   tab;;

let sequence_rwy = fun tab n ->
  let tab_sub = Array.make n 0 in
  let reste = (Array.length tab) mod n in
  let quotient = (Array.length tab) / n in
  for i=0 to quotient do
    for j 

