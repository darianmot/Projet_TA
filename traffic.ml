(* Compile commande : ocamlc -o traffic.out str.cma traffic.ml *)

let step = 5;; (* Temps (en s) s'ecoulant entre 2 positions successives *)


type position = {
  x : int;
  y : int;
};;
(* Type position (x,y) *)

type size = L | M | H;;
(*Type categorie d'avion *)

let size_of_string s = if s = "L" then L else if s = "M" then M else H;;
(* Associe un caractere à une chaine *)

type flight_type = DEP | ARR;;
(* Type depart ou arrivee *)

let flight_type_of_string s = if s = "ARR" then ARR else DEP;;
(* Associe une chaine a un type de vol *)

type cfmu =
  |None
  |Tcfmu of int;;
(* Type creneau cfmu (valable que pour certains departs) *)

let cfmu_of_string s = if s = "_" then None else Tcfmu (int_of_string s);;
(* Associe à une chaine un cfmu *)

type flight = {
  typ : flight_type;
  callsign : string;
  size : size;
  parking : string;
  runway : string;
  t_debut : int;
  t_rwy : int;
  t_eff : int;
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
   t_eff = 0;
   t_cfmu = t_cfmu;
   traj = traj};;
(* Enregistre un nouveau vol *)

let pos_of_string chaine =
  let l = Str.split (Str.regexp ",") chaine
  in let x = int_of_string (List.hd l)
     in let y = int_of_string (List.hd (List.tl l))
        in newPosition x y;;
(*Convertie un string "x,y" en une instance position*)

(*Debut acesseurs des attributs d'un vol*)
let getTyp flight = flight.typ;;
let getCallsign flight = flight.callsign;;
let getSize flight = flight.size;;
let getParking flight = flight.parking;;
let getRunway flight = flight.runway;;
let getT_debut flight = flight.t_debut;;
let getT_rwy flight = flight.t_rwy;;
let getT_cfmu flight = flight.t_cfmu;;
let getTraj flight = flight.traj;;
let getT_eff flight = flight.t_eff;;

let get_typ flight = flight.typ;;
let get_callsign flight = flight.callsign;;
let get_size flight = flight.size;;
let get_parking flight = flight.parking;;
let get_runway flight = flight.runway;;
let get_t_debut flight = flight.t_debut;;
let get_t_rwy flight = flight.t_rwy;;
let get_t_cfmu flight = flight.t_cfmu;;
let get_traj flight = flight.traj;;
let get_t_eff flight = flight.t_eff;;

(*Fin acesseurs des attributs d'un vol*)

let getT_fin flight =
  (getT_debut flight) + (List.length (getTraj flight))*step;;
(* Renvoie l'heure de fin de mouvement d'un fligth *)

let copy_flight flight t_rwy newtraj =
  {typ = getTyp flight;
   callsign = getCallsign flight;
   size = getSize flight;
   parking = getParking flight;
   runway = getRunway flight;
   t_debut = getT_debut flight;
   t_rwy = t_rwy;
   t_eff = getT_eff flight;
   t_cfmu = getT_cfmu flight;
   traj = newtraj};;
(* Renvoie une copie d'un flight ayant pour trajectoire newtraj et une heure de debut de piste t_rwy*)

let change_t_eff flight t_eff =
  {typ = getTyp flight;
   callsign = getCallsign flight;
   size = getSize flight;
   parking = getParking flight;
   runway = getRunway flight;
   t_debut = getT_debut flight;
   t_rwy = get_t_rwy flight;
   t_eff = t_eff;
   t_cfmu = getT_cfmu flight;
   traj = getTraj flight};;
(* Renvoie un copie de flight en changeant le temps effectif en t_eff *)

let change_t_debut flight t_debut =
  {typ = getTyp flight;
   callsign = getCallsign flight;
   size = getSize flight;
   parking = getParking flight;
   runway = getRunway flight;
   t_debut = t_debut;
   t_rwy = get_t_rwy flight;
   t_eff = getT_eff flight;
   t_cfmu = getT_cfmu flight;
   traj = getTraj flight};;
(* Renvoie un copie de flight en changeant le temps effectif en t_eff *)

let pos_at_time t flight =
  let traj = getTraj flight in
  let t_debut = getT_debut flight in
  let rec aux pos_list time = match pos_list with
    |[] -> raise Not_found
    |pos::q ->
       if time <= t && (time + step) > t
       then pos
       else aux q (time + step)
  in aux traj t_debut;;
(* Renvoie la position d'un vol à t ou leve l'exception Not_found si l'avion n'est pas/plus en mouvement *)                         

let is_parked t flight =
  try let pos = pos_at_time t flight in
      if (getTyp flight) == DEP
      then  List.hd (getTraj flight) = pos   (* On est encore au parking *)
      else List.hd (List.rev (getTraj flight)) = pos   (* On est déjà au parking *)
  with Not_found -> false;;
(* Renvoie true si le vol est au parking à la date t, false sinon *)
           

let rec flight_at_t t flight_l =  match flight_l with
  |[] -> []
  |flight::q ->
     try let pos = pos_at_time t flight in
         (flight, pos)::(flight_at_t t q);
     with Not_found -> flight_at_t t q;;
(* Renvoie la liste (flight, position du flight à t) des avions en mouvements dans la liste des vols flight_l à l'instant t  *)

let rec traffic_at_t t flight_l = match flight_l with
  |[] -> []
  |flight::q ->
     try let pos = pos_at_time t flight in
         pos::(traffic_at_t t q);
     with Not_found -> traffic_at_t t q;;
(* Renvoie la liste des positions des avions en mouvements dans la liste des vols flight_l à l'instant t *)

let separation = fun i j -> 
	match (get_size i,get_size j) with 
	(H,L) -> 180 
      | (M,L) -> 180
      | (H,M) -> 120 
      | (H,H) -> 90
      | (_,_) -> 60;;

let print_traj traj = 
  let print_pos pos =
    Printf.printf "(%d,%d) " pos.x pos.y in
  List.iter print_pos traj;;
(* Printf d'une trajectoire *)


let rec print_traffic l =
  match l with
  |[] -> Printf.printf ""
  |f::q -> begin
    let typ = if (getTyp f) = DEP then "DEP" else if  (getTyp f) = ARR then "ARR" else "???" in  Printf.printf "%s " typ;
    Printf.printf "%s " (getCallsign f);
    let size = if (getSize f) = L then "L" else if  (getSize f) = M then "M" else "H" in  Printf.printf "%s " size;
    Printf.printf "%s " (getParking f);
    Printf.printf "%s " (getRunway f);
    Printf.printf "%d " (getT_debut f);
    Printf.printf "%d " (getT_rwy f);
    let cfmu = match getT_cfmu f with
      |None -> "_"
      |Tcfmu t -> string_of_int t
         in Printf.printf "%s " cfmu;
    print_traj (getTraj f);
    Printf.printf "\n";
    print_traffic q end;;
(* Printf de la liste des vols passés en parametre *)



let read filename =
  let channel = open_in filename
  in let list = ref [] in
     try
    while true do  
      let line = input_line channel in 
      match Str.split (Str.regexp " ") line with
      |typ_s::callsign::size_s::parking::runway::t_debuts::t_rwys::t_cfmus::trajs -> 
       let typ = flight_type_of_string typ_s
       in let size = size_of_string size_s 
       in let t_debut = int_of_string t_debuts
       in let t_rwy =  int_of_string t_rwys
       in let t_cfmu = cfmu_of_string t_cfmus
       in let traj = List.map pos_of_string trajs
       in let f = newFlight  typ callsign size parking runway t_debut t_rwy t_cfmu traj
       in list := f::(!list)
      |_ -> ()
    done;
    !list;
     with End_of_file -> close_in channel;
       List.rev !list ;;
(* Renvoie une liste de flight contenu dans un fichier *)


                                

