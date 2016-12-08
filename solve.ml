(* Compile commande :  ocamlc -o solve str.cma traffic.cmo solve.ml *)

open Traffic;;

let min_dist = 70;; (* Distance minimale de séparation *)

let distance pos1 pos2 =
  sqrt (float (pos1.x - pos2.x) ** 2. +.  float (pos1.y - pos2.y) ** 2.) ;;
(* Distance en metre entre 2 position *)

let taxi_rule pos1 pos2 = distance pos1 pos2 >= (float min_dist);;
(* Renvoie true si 2 postions sont suffisamment separees au roulage *)

let explore = fun t p ->
  if p = length route then true
  else if p > 0 then match (t,p) with
    Marked -> False
    Unmarked -> Situation.mark
    Conflict -> False
  if explore (t+1) (p+1) then true
  else if explore (t+1) p then true
  else 

