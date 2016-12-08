open Traffic;;
open Airport;;

let _file = "data/lfpg_flights.txt";;

let load = read _file;;
(* Charge la liste des vols du fichier _file (global) *)

let lfpg = read_airport "data/lfpg_map.txt";;
print_airport lfpg;;

(*Tests
let traf = traffic_at_t 6500 load;;
let print_pos pos =
  Printf.printf "%d, %d\n" pos.x pos.y;;
  List.iter print_pos traf;;*)

for i=1 to 10 do 
  let traj  = Solve.resolution (List.nth  load i) (List.tl load) in 
    Printf.printf "%d %d \n" (List.length traj) (List.length (getTraj (List.nth load i)))
  done;;
