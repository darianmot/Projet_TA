
(* Compile commande : ocamlc -o traffic.out str.cma traffic.ml *)

let _file = "data/lfpg_flights.txt";;

type position = {
  x : int;
  y : int;
};;
(* Type position (x,y) *)

type size = L | M | H;;
(*Type categorie d'avion *)

type flight_type = DEP | ARR;;
(* Type depart ou arrivee *)

type cfmu =
  |None
  |Tcfmu of int;;
(* Type creneau cfmu (valable que pour certains departs) *)

type flight = {
  typ : flight_type;
  callsign : string;
  size : size;
  parking : string;
  runway : string;
  t_debut : int;
  t_rwy : int;
  t_cfmu : cfmu;
  traj : position list;
};;
(* Type vol *)

let newPosition x y =
  {x = x;
   y = y};;
(* Enregistre une nouvelle position *)

let newFlight typ callsign size parking runway t_debut t_rwy t_cfmu traj =
  {typ = typ;
   callsign = callsign;
   size = size;
   parking = parking;
   runway = runway;
   t_debut = t_debut;
   t_rwy = t_rwy;
   t_cfmu = t_cfmu;
   traj = traj};;
(* Enregistre un nouveau vol *)

(*Debut acesseurs des attributs d'un vol*)
let get_typ flight = flight.typ;;
let get_callsign flight = flight.callsign;;
let get_size flight = flight.size;;
let get_parking flight = flight.parking;;
let get_runway flight = flight.runway;;
let get_t_debut flight = flight.t_debut;;
let get_t_rwy flight = flight.t_rwy;;
let get_t_cfmu flight = flight.t_cfmu;;
let get_traj flight = flight.traj;;
(*Fin acesseurs des attributs d'un vol*)

let rec print_traffic l =
  match l with
  |[] -> Printf.printf ""
  |f::q -> begin
    let typ = if (get_typ f) = DEP then "DEP" else if  (get_typ f) = ARR then "ARR" else "???" in  Printf.printf "%s " typ;
    Printf.printf "%s " (get_callsign f);
    let size = if (get_size f) = L then "L" else if  (get_size f) = M then "M" else "H" in  Printf.printf "%s " size;
    Printf.printf "%s " (get_parking f);
    Printf.printf "%s " (get_runway f);
    Printf.printf "%d " (get_t_debut f);
    Printf.printf "%d " (get_t_rwy f);
    let cfmu = match get_t_cfmu f with
      |None -> "_"
      |Tcfmu t -> string_of_int t
         in Printf.printf "%s " cfmu;
    let pos1 =  List.hd (get_traj f) in Printf.printf "(%d, %d)\n" pos1.x pos1.y;
    print_traffic q end;;
(* Printf de la liste des vols passés en parametre (premiere position uniquement) *)


let read filename =
  let channel = open_in filename
  in let list = ref [] in
     try
    while true do  
      let line = input_line channel in 
      match Str.split (Str.regexp " ") line with
      |typ_s::callsign::size_s::parking::runway::t_debuts::t_rwys::t_cfmus::trajs -> 
       let typ = if typ_s = "ARR" then ARR else DEP
       in let size = if size_s = "L" then L else if size_s = "M" then M else H
       in let t_debut = int_of_string t_debuts
       in let t_rwy =  int_of_string t_rwys
       in let t_cfmu =  if t_cfmus = "_" then None else  Tcfmu (int_of_string t_cfmus)
       in let string_to_pos chaine = let l = Str.split (Str.regexp ",") chaine
                                        in let x = int_of_string (List.hd l)
                                        in let y = int_of_string (List.hd (List.tl l))
                                        in newPosition x y
       in let traj = List.map string_to_pos trajs
       in let f = newFlight  typ callsign size parking runway t_debut t_rwy t_cfmu traj
       in list := f::!list
      |_ -> ()
    done;
    !list;
     with End_of_file -> close_in channel;
       List.rev !list ;;
(* Renvoie une liste de flight contenu dans un fichier *)


                                

(****************************************************************************)
let fusion tab deb1 fin1 fin2 =
  let deb2 = fin1 + 1 and tab_bis = (Array.make (fin1 - deb1 + 1) tab.(0)) in
  let compt1 = ref(deb1) and compt2 = ref(deb2) in
  
  for i = deb1 to fin1 do
    tab_bis.(i-deb1) <- tab.(i) (* recopie de la premiére partie du tableau *)
  done;
  
  for i=deb1 to fin2 do
    if (!compt1) = deb2 then
      ()
    else if (!compt2) = (fin2 + 1) then
      begin
        tab.(i) <- tab_bis.(!compt1 - deb1);
        compt1 := (!compt1) + 1
      end
    else
      if (tab_bis.(!compt1 - deb1)).t_rwy < (tab.(!compt2)).t_rwy then
	begin
          tab.(i) <- tab_bis.(!compt1-deb1);
          compt1 := (!compt1)+1
	end
    else
      begin
        tab.(i) <- tab.(!compt2);
        compt2 := (!compt2)+1;
      end
  done;;
(*  val fusion : int array -> int -> int -> int -> unit = <fun> *)
  
let tri_fusion tab =

  let rec tri_fusion_bis tab debut fin =
    if debut <> fin then
      begin
        let milieu = (debut+fin)/2 in
        tri_fusion_bis tab debut milieu;
        tri_fusion_bis tab (milieu+1) fin;
        fusion tab debut milieu fin;
      end;

  in
  let longueur = (Array.length tab) in
    if (longueur > 0) then
      tri_fusion_bis tab 0 (longueur-1);;

(* val tri_fusion : int array -> unit = <fun> *)

let echanger = fun tab i j -> 
	let memoire = tab.(i) in 
	tab.(i) <- tab.(j);
	tab.(j) <- memoire;;
		
let separation = fun i j -> 
	match (get_size i,get_size j) with 
	(H,L) -> 180 
      | (M,L) -> 180
      | (H,M) -> 120 
      | (H,H) -> 90
      | (_,_) -> 60;;


let list_to_tab = fun list ->
  Array.of_list list;;



let impr = fun tab ->
  let size = Array.length tab in
  for i = 0 to (size-1) do
    let flight = tab.(i) in
    Printf.printf "%d\n" (get_t_rwy flight) done
;;


let load () = read _file;;
(* Charge la liste des vols du fichier _file (global) *)
load();;

(*print_traffic (load ());;*)

let tableau = list_to_tab (load ());;
tableau;;

tri_fusion tableau;;

impr tableau;;
 (*
let sequence = fun retard i ->
  let ref borne = max_int in
  let rec sequence_rec = fun born retard i ->
  let n = Array.length in
  if i < n
  then
    begin
    for j = i to (n-1) do
      if (i=j) || (separation tab.(i) tab.(j) >  separation tab.(j) tab.(i))
      then
	begin
	  if (i != j)
	  then
	    echanger tab.(i) tab.(j);	  
	      if (i = 0)
	      then
		tab.(i).t_rway =  get_t_rwy tab.(i) 
	      else
		let ti = tab.(i).t_rway in
		tab.(i).t_rway = max (get_t_rway tab.(i)) (get_t_rway tab.(i-1) + (separation tab.(i-1) tab.(i)));
	 retard = retard + ti - tab.(i).t_rway;
	 if retard < !borne
	 then
	  sequence_rec retard (i+1)
	end
    end
  else 
  borne := retard in

  sequence_rec borne retard i;;*)
		    
		  
       
    
