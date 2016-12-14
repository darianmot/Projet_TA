open Traffic;;
open Graphics;;
open Airport;;

let max_coord_x = 5000;;
let max_coord_y = 3500;;
let airport_color = cyan;;
let flight_color = black;;

let pos_to_pix pos = 
  let win_x =  size_x () in
  let win_y = size_y () in
  let x = int_of_float ( (float pos.x /. float max_coord_x) *. (float win_x /. 2.) ) + win_x / 2   in
  let y = int_of_float ( (float pos.y /. float max_coord_y) *. (float win_y /. 2.) ) + win_y / 2 in
  (x, y);;
(* Convertie les coord d'un point en coord pixel *)

let draw_pos pos rayon =
  let (x,y) = pos_to_pix pos in
  draw_circle x y rayon;;
(* Dessine une position *)

let draw_traffic_at_t t flight_l =
  let d_pix = int_of_float ( (float 70 /. float max_coord_x) *. (float (size_x ()) /. 2.) ) in
  let todraw = traffic_at_t t flight_l in
  let aux  pos = draw_pos pos (d_pix/2) in
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

let plot_airport airport =
 List.iter plot_rwy airport.runways;
List.iter plot_taxi airport.taxiways;;
(* Plot un airport *)

let start t_init flight_l airport = 
  open_graph "";
  set_color airport_color;
  plot_airport airport;
  let background = ref (get_image 0 0 (size_x ()) (size_y ())) in
  let t = ref t_init in
  set_color flight_color;
  try
    while true do
      draw_image !background 0 0;
      draw_traffic_at_t !t flight_l;    
      begin
        match read_key () with
        |'q' -> raise Exit
        |'-' -> t := !t - step
        |_-> t := !t + step
      end;
      clear_graph ();
    done
  with Exit -> ();;
(* Lance l'animation *)

