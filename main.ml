open Traffic;;
open Airport;;

let _file = "data/lfpg_flights_light.txt";;

let load = read _file;;
(* Charge la liste des vols du fichier _file (global) *)

(*let lfpg = read_airport "data/lfpg_map.txt";;
  print_airport lfpg;;*)


let new_load = Solve.resolution load;;

Plot.start 5100 new_load;;
