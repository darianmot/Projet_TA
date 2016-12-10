open Traffic;;
open Graphics;;

let max_coord_x = 5000;;
let max_coord_y = 4000;;

let init () = open_graph "";;

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
  let todraw = flight_at_t t flight_l in
  let aux (flight, pos) = draw_pos pos (d_pix/2) in
  List.iter aux todraw;;
(* Dessine le traffic d'une liste de vols à t *)

let start t_init flight_l= 
  init ();
  let t = ref t_init in
  try
    while true do
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
