(* Compile commande :  ocamlc -o solve str.cma traffic.cmo solve.ml *)

open Traffic;;

let min_dist = 70;; (* Distance minimale de séparation *)

let distance pos1 pos2 =
  sqrt (float (pos1.x - pos2.x) ** 2. +.  float (pos1.y - pos2.y) ** 2.) ;;
(* Distance en metre entre 2 position *)

let taxi_rule pos1 pos2 = distance pos1 pos2 >= (float min_dist);;
(* Renvoie true si 2 postions sont suffisamment separees au roulage *)

let conflicted_roulage t pos traffic =
  let roulage bool (flight, position) = bool ||  (not (taxi_rule pos position)) in
  List.fold_left roulage false traffic;;
  (* conflit de seperation roulage *)

let conflicted_turbulence f t p p_rwy traffic = 
    if (getTyp f = DEP) && p >= p_rwy then
      let rec piste l = match l with
        |[] -> false
        |flight::q when (getRunway f) = (getRunway flight) ->
           let sep = separation f flight in
           if ((getTyp flight) = DEP && (t - sep) >= getT_fin flight) ||
             ((getTyp flight) = ARR && (t - sep) >= getT_rwy flight)
           then piste q
           else true
        |t::q -> piste q
      in piste traffic
    else false;;
(* conflit de separation piste avec f flight a traiter, p le numero de position actuel*)

(* Renvoie true si la position est conflictuelle a t, false sinon *)

let rec resolution flight_l =
  let rec aux retard solved_flight flight_to_solve =
    match flight_to_solve with
    |[] -> (List.rev solved_flight, retard)
    |flight::q ->
       let debut = (getT_debut flight) in
       let route = getTraj flight in 
       let n = List.length route in
       let tmin = Array.make n (-max_int) in
       let traj = ref [] in
       let p_rwy = ((getT_rwy flight) - debut) / step in
       let rec explore = fun arbre pos_l ->
         match arbre with
         |(_,p,_) when p = n -> true
         |(t, p, tmin) when t <= tmin.(p) -> false 
         |(t, p, tmin) ->
            let pos = List.hd pos_l in 
            begin
              tmin.(p) <- t;
              let traffic = flight_at_t t solved_flight in
              let parked = if getTyp flight = DEP then p = 0 else false in (* A modifier *)
              if (not parked) && ( conflicted_roulage t pos traffic || conflicted_turbulence flight t p p_rwy solved_flight )
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
           let newF = copy_flight flight tmin.(p_rwy) (List.rev !traj) in
           let new_retard = retard + (getT_fin newF - getT_fin flight) in
           aux new_retard (newF::solved_flight) q
         else begin Printf.printf "+++ Fail to solve %s +++" (getCallsign flight); (solved_flight, retard) end;
  in aux 0 [] flight_l;;
(* A partir d'une liste de vols seqencee, renvoie la liste des vols sans conflits et le retard total engendre *)          
  
        


