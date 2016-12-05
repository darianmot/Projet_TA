(* Compilation : ocamlc -o airport str.cma traffic.cmo airport.ml *)

open Traffic;;

let pt_key = "P";;
let taxi_key = "L";;
let rwy_key = "R";;
let conf_key = "C";;
(* Carcatere caracterisant le type de donnee d'une ligne à traiter *)

let oneway_char = "S";;
(* Caractere definissant si un taxiway est à sens unique *)

type point_type = Stand | Deicing | Rwy;;
(* Type de point Parking | Degel??? | Point de sortie *)

let point_type_of_int n = match n with
  |0 -> Stand
  |1 -> Deicing
  |_ -> Rwy
(* Associe un entier à un type de point *)

type point = {
  name_point : string;
  type_point : point_type;
  position_point : position;
};;
(* Point de l'aeroport *)

type taxiway = {
  name_taxiway : string;
  speed_taxiway : int;
  size_taxiway : size;
  oneway_taxiway : bool;
  position_taxiway : position list;
};;
(* Taxiway de l'aeroport *)

type runway = {
  name_runway : string;
  qfus : string*string;
  position_runway : position list; 
  runway_points : string list;  (*Noms des points de la piste *)
};;
(* Runway de l'aeroport *)

type config = {
  name_config : string;
  dep_qfu : (string * (string list)) list;
  arr_qfu : (string * (string list)) list;
};;
(* Configuration aeroport (qfus en service) *)


type airport = {
  name_airport : string;
  points: point list;
  taxiways : taxiway list;
  runways : runway list;
  configs : config list;
  numero_config : int;
};;
(* Type airport *)

(*Debuts constructeurs*)
let newPoint name typ position =
  {name_point = name;
   type_point = typ;
   position_point = position;
  };;

let newTaxiway name speed size oneway position = {
  name_taxiway = name;
  speed_taxiway = speed;
  size_taxiway =  size;
  oneway_taxiway = oneway;
  position_taxiway = position;
};;

let newRunway name qfus points position = {
  name_runway = name;
  qfus = qfus;
  position_runway = position; 
  runway_points = points;
};;

let newConfig name dep arr = {
  name_config = name;
  dep_qfu = dep;
  arr_qfu = arr;
};;

let newAirport name pts twys rwys confs num = {
  name_airport = name;
  points = pts;
  taxiways = twys;
  runways = rwys;
  configs = confs;
  numero_config = num;
};;
  
(*Fin constructeurs *)

let read_airport filename =
  let channel = open_in filename in
  let points = ref [] in
  let taxiways = ref [] in
  let runways = ref [] in
  let configs = ref [] in
  let airport_name = input_line channel in
        try
          while true do  
            let line = input_line channel in 
            match Str.split (Str.regexp " ") line with
            |key::name::typ_s::pos_l when key = pt_key ->
               let typ = int_of_string typ_s in
               let pt = (newPoint name (point_type_of_int typ) (pos_of_string (List.hd pos_l))) in
                   points := pt::(!points)
            |key::name::speed_s::size_s::oneway_s::pos_l when key = taxi_key ->
               let speed = int_of_string speed_s in
               let size = size_of_string size_s in
               let oneway = (oneway_s = oneway_char) in
               let pos = List.map pos_of_string pos_l in
               let txy = (newTaxiway name speed size oneway pos) in
               taxiways := txy::(!taxiways)
            |key::name::qfu1::qfu2::pt_s::pos_l when key = rwy_key ->
               let pt = Str.split (Str.regexp ",") pt_s in
               let pos = List.map pos_of_string pos_l in
               let rwy = newRunway name (qfu1,qfu2) pt pos in
               runways := rwy::(!runways)
               |key::name::qfus_l when key = conf_key->
                  let rec qfus_of_strings list_qfus dep arr = match list_qfus with
                    |[] -> (dep, arr);
                    |t::q  -> let qfu = Str.split (Str.regexp ",") t
                              in match qfu with
                              |typ_s::name::points -> if flight_type_of_string typ_s == ARR
                                then qfus_of_strings q dep ((name,points)::arr)
                                else qfus_of_strings q ((name,points)::dep) arr
                              |_ ->  qfus_of_strings q dep arr  (* Cas d'un mauvais format *)
                  in let (dep_qfu, arr_qfu) = qfus_of_strings qfus_l [] []
                     in configs := (newConfig name dep_qfu arr_qfu)::(!configs)
            |_ -> () (* Cas d'un mauvais format *)
          done;
          newAirport "XXX" [] [] [] [] 0; (* Pour renvoyer le meme type dans le try et dans le with *)
        with 
          End_of_file -> begin
            close_in channel;
            newAirport airport_name !points !taxiways !runways !configs 0;
          end;;
(* Lis un fichier et renvoie l'aeroport associé *)


let print_airport airport =
  let np = (List.length airport.points) in
  let nt = (List.length airport.taxiways) in
  let nr = (List.length airport.runways) in
  let nc = (List.length airport.configs) in 
  Printf.printf "%s\nPoints : %d\nTaxiways : %d\nRunways :  %d\nConfigs : %d\n" airport.name_airport np nt nr nc;; 
(* Affichage aeroport *)



let lfpg = read_airport "data/lfpg_map.txt";;
print_airport lfpg;;
