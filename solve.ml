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
  let f bool pos2 = bool && (not (taxi_rule pos pos2)) in
  List.fold_left f true traffic;;

type noeud = int*(position list)*(int ref);;


let resolution flight flight_l =
  let debut = (getT_debut flight) in
  let route = List.tl (getTraj flight) in
  let tmin = ref max_int in
  let traj = ref [] in
  let rec explore = fun arbre  ->
    match arbre with
    |(_,[],_) -> true
    |(t, _, tmin) when t > !tmin -> false
    |(t, pos::q, tmin) ->
       begin
         tmin := t;
         if (conflicted_pos t pos flight_l)
         then false
         else
           begin
             traj := pos::(!traj);
             if explore (t+step, q, ref max_int) then true
             else
               begin
                 if explore (t+step, pos::q, tmin)
                 then true
                 else begin print_traj !traj ;traj := (List.tl !traj); false end
               end
           end
       end
  in explore (debut, route, tmin);
  List.rev !traj;;
             
  
        


