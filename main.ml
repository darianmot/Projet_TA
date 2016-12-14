open Traffic;;
open Airport;;

let _file = "data/lfpg_flights_light.txt";;

let load = read _file;;
(* Charge la liste des vols du fichier _file (global) *)

let lfpg = read_airport "data/lfpg_map.txt";;

let t1 = Sys.time ();;
let (new_load, retard) = Solve.resolution load lfpg;;
let t2 = Sys.time ();;
Printf.printf "Retard total : %d\n" retard;;
Printf.printf "Temps : %f\n" (t2 -. t1);;
Plot.start 5000 new_load lfpg;;
