open Traffic;;
open Graphics;;
open Airport;;

let max_coord_x = 5000;; 
let max_coord_y = 3500;;
(* Definissent l'echelle utilisee pour tracer l'aeroport *)

let taxi_color = cyan;;
let rwy_color = rgb 192 192 192;;
let light_color = blue;;
let medium_color = red;;
let heavy_color = black;;
let text_color = black;;
(* Couleurs *)

let flight_size = Solve.min_dist;;
(* Taille d'un plot (en metre) *)

let vitesse_lecture = 1.;;
(* Vitesse de lecture relative *)

let pos_to_pix pos = 
  let win_x =  size_x () in
  let win_y = size_y () in
  let x = int_of_float ( (float pos.x /. float max_coord_x) *. (float win_x /. 2.) ) + win_x / 2   in
  let y = int_of_float ( (float pos.y /. float max_coord_y) *. (float win_y /. 2.) ) + win_y / 2 in
  (x, y);;
(* Convertie les coord d'un point en coord pixel *)

let m_to_pix lg =
  int_of_float ( (float lg /. float max_coord_x) *. (float (size_x ()) /. 2.) );;
(* Convertie un donnee metre en donnee pixel *)  

let draw_pos pos rayon size =
  let (x,y) = pos_to_pix pos in
  begin
    match size with
    |L -> set_color light_color;
    |M -> set_color medium_color
    |H -> set_color heavy_color;
  end;
  draw_circle x y rayon;
  fill_circle x y rayon;
  set_color text_color;;
(* Dessine un flight a un instant *)


let draw_traffic_at_t t flight_l size =
  let f_size = float (m_to_pix flight_size) *. size /. 2. in
  let todraw = flight_at_t t flight_l in
  let aux (flight, pos) = draw_pos pos (int_of_float f_size) (getSize flight) in
  List.iter aux todraw;;
(* Dessine le traffic d'une liste de vols à t *)


let plot_taxi taxi =
  let pix_l = List.map pos_to_pix taxi.position_taxiway in
  let rec aux l =
    match l with
    |[] -> ()
    |[x] -> ()
    |(x0,y0)::(x1,y1)::q ->
       begin
         draw_segments [|(x0,y0,x1,y1)|];
         aux ((x1,y1)::q)
       end
  in aux pix_l;;
(* Plot un taxiway *)

let plot_rwy rwy =
  let pix_l = List.map pos_to_pix rwy.position_runway in
  match pix_l with
  |(x0,y0)::(x1,y1)::q -> draw_segments [|(x0,y0,x1,y1)|]
  |_ -> ();;
(* Plot une piste *)

let plot_airport airport =
  set_color taxi_color;
  List.iter plot_taxi airport.taxiways;
  set_color rwy_color;
  set_line_width 5;
  List.iter plot_rwy airport.runways;
  set_line_width 1;;
(* Plot un airport *)

let wait sec =
  let t0 = Sys.time () in
  let t = ref (Sys.time ()) in
  while (!t -. t0) < sec do
    t:= Sys.time ()
  done;;
(* Permet de mettre en pause le programme un nombre flottant de seconde *)

let plot_background airport sizex sizey =
  let indication = "p = pause; q= quitter " in
  let (size_text_x, size_text_y) = text_size indication in
  moveto 1 (sizey  - size_text_y);
  draw_string indication;
  plot_airport airport;
  get_image 0 0 sizex sizey;;
(* Dessine l'aeroport ET renvoie l'image associee *)

let start t_init flight_l airport = 
  open_graph "";
  let sizex = ref (size_x ()) in
  let sizey = ref (size_y ()) in
  let background = ref (plot_background airport !sizex !sizey) in
  let t = ref t_init in
  let auto = ref true in
  try
    while true do
      if !sizex != size_x () ||  !sizey != size_y () then 
        begin
          sizex := size_x ();
          sizey := size_y ();
          background := plot_background airport !sizex !sizey
        end
      else (); (* On redimensionne le fond si la taille de la fenetre change *)
      draw_image !background 0 0;
      moveto 1 0;
      draw_string ("Temps : " ^ string_of_int !t);
      draw_traffic_at_t (!t - 2*step) flight_l 0.5;
      draw_traffic_at_t (!t - step) flight_l 0.75;
      draw_traffic_at_t !t flight_l 1.; (* Affichage du temps courant et du traffic *)
      begin
      if !auto then
        let status = wait_next_event [Poll] in
        if status.keypressed then
          begin
            match status.key with
            |'q' -> raise Exit
            |'p' -> auto := false;
              ignore (wait_next_event [Key_pressed]);
            |_ -> ignore (wait_next_event [Key_pressed]);
          end
        else 
          begin
            wait (0.02 /. vitesse_lecture);
            t := !t + step;
          end
      else
        let avancer = "+ = avancer" in
        let reculer = "- = reculer" in
        let (size_text_x, size_text_y) = text_size avancer in
        moveto 1  (size_y () - 2*size_text_y);
        draw_string avancer;
        moveto 1  (size_y () - 3*size_text_y);
        draw_string reculer;
        begin 
          let key = read_key () in
          match key with
          |'q' -> raise Exit
          |'p' -> auto := true 
          |'-' -> t := !t - step  
          |'+' -> t := !t + step
          |_ -> ()
        end
      end;
      clear_graph ();
    done;
  with Exit -> ();;
(* Lance l'animation *)

