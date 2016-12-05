(* Compile commande :  ocamlc -o solve str.cma traffic.cmo solve.ml *)

open Traffic;;

let min_dist = 70;; (* Distance minimale de séparation *)

let distance pos1 pos2 =
  sqrt (float (pos1.x - pos2.x) ** 2. +.  float (pos1.y - pos2.y) ** 2.) ;;
(* Distance en metre entre 2 position *)

let taxi_rule pos1 pos2 = distance pos1 pos2 >= (float min_dist);;
(* Renvoie true si 2 postions sont suffisamment separees au roulage *)

let rec traffic_at_t t flight_l = match flight_l with
  |[] -> []
  |flight::q ->
     try let pos = pos_at_time t flight in
         pos::(traffic_at_t t q);
     with Not_found -> traffic_at_t t q;;
(* Renvoie la liste des positions des avions en mouvements à l'instant t *)

(*Tests*)
let traf = traffic_at_t 2000 (load ());;
let print_pos pos =
  Printf.printf "%d, %d\n" pos.x pos.y;;
List.iter print_pos traf;;
