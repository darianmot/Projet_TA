(* Compile commande : ocamlc -o traffic.out str.cma traffic.ml *)


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
  runway : string;
  traj : string list;
};;
(* Type vol *)

let newFlight typ callsign size runway traj =
  {typ = typ;
   callsign = callsign;
   size = size;
   runway = runway;
   traj = traj};;
(* Enregistre un nouveau vol *)


let rec print_traffic l =
  match l with
  |[] -> Printf.printf ""
  |f::q -> begin Printf.printf "%s\n" f.callsign; print_traffic q end;;
(* Printf de la liste des vols passés en parametre *)


let read filename =
  let channel = open_in filename in
  let rec read_line acc =
    match Str.split (Str.regexp " ") (input_line channel) with
    |typ_s::callsign::size_s::runway::traj ->
       let typ = if typ_s == "dep" then DEP else ARR
       in let size = if size_s == "L" then L else if size_s == "M" then M else H 
       in let f = newFlight typ callsign size runway traj
       in read_line (f::acc);
    |l -> read_line acc
    |exception End_of_file -> acc
  in read_line [];;
(* Renvoie une liste de flight contenu dans un fichier *)

print_traffic (read "data/lfpg_flights.txt");;
