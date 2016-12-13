open Traffic;;
open Airport;;

let _file = "data/lfpg_flights_light.txt";;

let load = read _file;;
(* Charge la liste des vols du fichier _file (global) *)

let lfpg = read_airport "data/lfpg_map.txt";;


let (new_load, retard) = Solve.resolution load lfpg;;
Printf.printf "%d\n" retard;;
Plot.start 5000 new_load lfpg;;
