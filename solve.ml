(* Compile commande :  ocamlc -o solve str.cma traffic.cmo solve.ml *)

open Traffic;;

let min_dist = 70;; (* Distance minimale de séparation *)

let distance pos1 pos2 =
  sqrt (float (pos1.x - pos2.x) ** 2. +.  float (pos1.y - pos2.y) ** 2.) ;;
(* Distance en metre entre 2 position *)

let taxi_rule pos1 pos2 = distance pos1 pos2 >= (float min_dist);;
(* Renvoie true si 2 postions sont suffisamment separees au roulage *)

let conflicted_pos t pos flight_l =
  let traffic = traffic_at_t t flight_l in
  let f bool pos2 = bool ||  (not (taxi_rule pos pos2)) in
  List.fold_left f false traffic;;


let rec resolution flight_l =
  let rec aux solved_flight flight_to_solve =
    match flight_to_solve with
    |[] -> solved_flight
    |flight::q ->
       let debut = (getT_debut flight) in
       let route = getTraj flight in 
       let n = List.length route in
       let tmin = Array.make n (-max_int) in
       let traj = ref [] in
       let rec explore = fun arbre pos_l ->
         match arbre with
         |(_,p,_) when p = n -> true
         |(t, p, tmin) when t <= tmin.(p) -> false 
         |(t, p, tmin) ->
            let pos = List.hd pos_l in 
            begin
              tmin.(p) <- t;
              let parked = if getTyp flight = DEP then p = 0 else false in
              if (not parked) && (conflicted_pos t pos solved_flight)  (* Si pos est conflictuelle et pas au parking *)
              then false
              else
                begin
                  traj := pos::(!traj);
                  if explore (t + step, p + 1 , tmin) (List.tl pos_l)  (* On essaye d'avancer *)
                  then true 
                  else
                    begin
                      if explore (t + step, p, tmin) pos_l  (* Sinon on attend et réessaye *)
                      then true
                      else begin traj := (List.tl !traj); false end
                    end
                end
            end
       in if explore (debut, 0, tmin) route
         then 
           let newFlight = changeTraj flight (List.rev !traj) in
           aux (solved_flight@[newFlight]) q
         else begin Printf.printf "+++ FAil %s +++" (getCallsign flight); solved_flight end;
  in aux [List.hd flight_l] (List.tl flight_l);;
             
  
        


