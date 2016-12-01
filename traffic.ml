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

type flight = {
  typ : flight_type;
  callsign : string;
  size : size;
  parking : string;
  runway : string;
  t_debut : int;
  t_rwy : int;
  t_cfmu : int;
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
    Printf.printf "%d " (get_t_cfmu f);
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
       in let t_cfmu =  if t_cfmus = "_" then -1 else int_of_string t_cfmus
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


                                
let load () = read _file;;
(* Charge la liste des vols du fichier _file (global) *)

print_traffic (load ());;
