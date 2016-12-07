open Traffic;;
open Airport;;

let _file = "data/lfpg_flights.txt";;

let load = read _file;;
(* Charge la liste des vols du fichier _file (global) *)

let lfpg = read_airport "data/lfpg_map.txt";;
print_airport lfpg;;

(*Tests*)
let traf = traffic_at_t 6500 load;;
let print_pos pos =
  Printf.printf "%d, %d\n" pos.x pos.y;;
List.iter print_pos traf;;
