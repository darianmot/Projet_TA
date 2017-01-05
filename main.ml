open Traffic;;
open Airport;;

let file = "data/lfpg_flights.txt";;

(*-----------------*)
(* Programs options *)
let line_to_read = ref (-1)
let vitesse_plot = ref 1.0
let debut_plot = ref 5000
  
let speclist = [
  ("-n", Arg.Set_int line_to_read, "Restriction du nombre de ligne du fichier à charger");
  ("-v", Arg.Set_float vitesse_plot, "Vitesse relative des plots de l'animation");
  ("-d", Arg.Set_int debut_plot, "Temps de debut de la simulation (en secondes)")
]
  
let anonymous_fun= fun s -> failwith (Printf.sprintf "There should be no anonymous arguments: %s" s)
  
let usage_msg= "Options available";;


(*---------------------*)
(* Programme principal *)
let () =

  (*Parse de la ligne de commande *)
  Arg.parse speclist anonymous_fun usage_msg;
  
  (* Chargement du traffic et de l'aeroport *)
  let load = read ~n:(!line_to_read) file in
  let lfpg = read_airport "data/lfpg_map.txt" in

  (* Resolution des conflits *)
  let t1 = Sys.time () in 
  let (new_load, retard) = Solve.resolution load lfpg in
  let t2 = Sys.time () in
  begin
    Printf.printf "Retard total : %d\n" retard;
    Printf.printf "Temps : %f\n" (t2 -. t1);
    flush_all ();
    Plot.start !debut_plot new_load lfpg !vitesse_plot;
  end;;


