open Traffic;;
let n_sequencement = 10;; (* Nombre d'avions à séquencer à la fois *)
(***********************************************)


let echanger = fun tab i j -> 
	let memoire = tab.(i) in 
	tab.(i) <- tab.(j);
	tab.(j) <- memoire;;
(* Echange deux cases d'un tableau *)

let list_to_tab = fun list ->
  Array.of_list list;;
(* Transforme une liste en tableau *)

let impr = fun tab ->
  let size = Array.length tab in
  Printf.printf "liste des vols : \n";
  for i = 0 to (size-1) do
    let flight = tab.(i) in
    let size = if (getSize flight) = L then "L" else if  (getSize flight) = M then "M" else "H" in
    Printf.printf "Callsign : %s  Runway : %s T_début : %d T_eff : %d T_rwy : %d Size : %s\n" (get_callsign flight) (get_runway flight) (get_t_debut flight) (get_t_eff flight) (get_t_rwy flight) size;
  done;;
(* Affiche la caractéristique de flight demandée (f) *)

let cmp_t_rwy = fun v1 v2 ->
  compare (get_t_rwy v1) (get_t_rwy v2);;
(* Compare deux vols par leur heure minimum d'entrée ou de sortie de la piste*)

let cmp_t_eff = fun v1 v2 ->
  compare (get_t_eff v1) (get_t_eff v2);;
(* Compare deux vols par leur heure de depart ou arrivée effective*)

let list_runway = fun tab ->
  let accARR = ref [] in
  let acc26R = ref [] in
  let acc27L = ref [] in
  for i=0 to ((Array.length tab)-1) do
    match (get_runway tab.(i)) with
      |"26R" -> acc26R := tab.(i)::(!acc26R)
      |"27L" -> acc27L := tab.(i)::(!acc27L)
      |_ -> accARR := tab.(i)::(!accARR);
  done;
  let res = [| !acc26R; !acc27L; !accARR |] in
  res;;
(* Crée 3 listes d'avions correspondant chacune soit au piste de DEP soit aux ARR *)


let sequence_opti = fun retard i n tab ->
  let borne = ref max_int in
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
  (!solution, !borne);;
(* Calcul de séquencement optimal par l'algorithme de Branch & Bound *)


let sequence_fcfs = fun tab ->
  let retard = ref 0 in
  for i = 0 to (Array.length tab-1) do
    if i = 0 then 
      tab.(i) <- change_t_eff tab.(i) (get_t_rwy tab.(i))
    else
      begin
	tab.(i) <- change_t_eff tab.(i) (max (get_t_rwy tab.(i)) (get_t_eff tab.(i-1) + separation tab.(i-1) tab.(i)));
	retard := !retard + (get_t_eff tab.(i)) - (get_t_rwy tab.(i));
      end;
  done;
  (tab, !retard);;
(* Calcul de séquencement First Come First Serve*) 

let sequence_opti_slot = fun tableau n ->
  let reste = (Array.length tableau) mod n in 
  let quotient = (Array.length tableau) / n in
  let retard_tot = ref 0 in
  for i=0 to (quotient-1) do
    let init = if i = 0 then 0 else (i*n)-1 in
    let (sol, retard) = sequence_opti 0 init (((i+1)*n)-1) tableau in
    Array.blit sol init tableau init n;
    retard_tot := !retard_tot + retard;
  done;
  let init_rest =  (quotient*n) in
  let (sol_rest, retard) = sequence_opti 0 (max (init_rest-1) 0) (init_rest + reste) tableau  in
  Array.blit sol_rest init_rest tableau init_rest reste ;
  retard_tot := !retard_tot + retard;
  (tableau, !retard_tot);
;;
(*Calcul du sequencement sur des périodes de n avions *)

let get_New_t_debut = fun tab ->
  for i = 0 to Array.length tab - 1 do
    let t_taxi= (get_t_rwy tab.(i) - get_t_debut tab.(i)) in
    let new_tdebut = (get_t_eff tab.(i)) - (t_taxi) in
    tab.(i) <- (change_t_debut tab.(i) new_tdebut);
    tab.(i) <- change_t_rwy tab.(i) (get_t_eff tab.(i));
  done;;

  

let runways traffic =
  let tab = list_to_tab traffic in
  let tri = list_runway tab in
  let rwy26R = list_to_tab tri.(0) in
  let rwy27L = list_to_tab tri.(1) in
  let rwyARR = list_to_tab tri.(2) in
  Array.sort cmp_t_rwy rwy26R;
  Array.sort cmp_t_rwy rwy27L;
  (rwy26R, rwy27L, rwyARR);;(* On trie les 2 listes précedentes DEP par leur heure d'entrée sur la piste*)
(* Renvoie les tableaux des pistes d'un trafic, trie par t_rwy pour les DEP *)

let seq_final_opti = fun load ->
  let (rwy27L, rwy26R, rwyARR) = runways load in
  let (seq27L, retard27L) = sequence_opti_slot rwy27L n_sequencement in
  let taille27L = (Array.length seq27L) in
  Printf.printf "retard 27L opti : %d taille : %d\n" retard27L taille27L;
  get_New_t_debut seq27L;
  let (seq26R, retard26R) = sequence_opti_slot rwy26R n_sequencement in
   let taille26R = (Array.length seq26R) in
  Printf.printf "retard 26R opti : %d taille : %d\n" retard26R taille26R;
  get_New_t_debut seq26R;
  
  let tab_final = Array.append seq27L (Array.append seq26R rwyARR) in
  Array.sort cmp_t_eff tab_final;
  let retard_seq = (retard26R+retard27L) in
  Printf.printf "retard_seq total opti : %d\n" retard_seq;
  (Array.to_list tab_final, retard_seq);;  

let seq_final_fcfs = fun load ->
  let (rwy27L, rwy26R, rwyARR) = runways load in
  let (seq27L, retard27L) = sequence_fcfs rwy27L in
  Printf.printf "retard 27L fifo : %d\n" retard27L;
  get_New_t_debut seq27L;
  
  let (seq26R, retard26R) = sequence_fcfs rwy26R in
  Printf.printf "retard 26R fifo : %d\n" retard26R;
  get_New_t_debut seq26R;
  
  let tab_final = Array.append seq27L (Array.append seq26R rwyARR) in
  Array.sort cmp_t_eff tab_final;
  let retard_seq = (retard26R+retard27L) in
  Printf.printf "retard_seq total fcfs : %d\n" retard_seq;
   (Array.to_list tab_final, retard_seq);;  
