(* Compile commande : ocamlc -o traffic.out str.cma traffic.ml *)

let _file = "data/lfpg_flights.txt";;
let step = 5;; (* Temps (en s) s'ecoulant entre 2 positions successives *)

exception Not_found;;
(* Exception levee si un element n'est pas trouve *)

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
(*Fin acesseurs des attributs d'un vol*)

let getT_fin flight =
  (getT_debut flight) + (List.length (getTraj flight))*step;;
(* Renvoie l'heure de fin de mouvement d'un fligth *)

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
    let pos1 =  List.hd (getTraj f) in Printf.printf "(%d, %d)\n" pos1.x pos1.y;
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


                                
let load () = read _file;;
(* Charge la liste des vols du fichier _file (global) *)
