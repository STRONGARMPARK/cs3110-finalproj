open Graphics
open Evolution1d
;;

let _ = open_graph ":0 720x720"

let sx = 40 

let sy = 320

let point x y = fill_circle x y 1
let point_size x y s = fill_circle x y s

let l = []

let _ = List.map (fun p -> point (fst p) (snd p)) l

let opx = (size_x () / 2)

let opy = (size_y () / 2)

let tick_length = 4

let tick_h opx opy y = 
  draw_poly_line [|(opx - tick_length / 2, opy + y * sy); 
                   (opx + tick_length / 2, opy + y * sy)|]

let tick_v opx opy x = 
  draw_poly_line [|(opx + x * sx, opy - tick_length / 2); 
                   (opx + x * sx, opy + tick_length / 2)|]

let generate_ticks opx opy = 
  let _ = for i = 0 to size_x () / sx do tick_v opx opy (i - opx / sx) done in
  for i = 0 to size_y () / sy do tick_h opx opy (i - opy / sy) done

let axis opx opy =
  let _ = draw_poly_line [|(0, opy);(size_x (), opy)|] in
  let _ = draw_poly_line [|(opx, 0);(opx, size_y ())|] in
  generate_ticks opx opy

let plot_func_bounds f lb ub = 
  for i = lb to ub do
    let x = float_of_int (i) /. float_of_int sx in
    let y = int_of_float (f x *. (float_of_int sy)) in
    point (i + opx) (y + opy) 
  done

let plot_func f bounds = 
  match bounds with
  | (None, None) -> plot_func_bounds f (-1 * size_x () / 2) (size_x () / 2)
  | (Some lb, None) -> plot_func_bounds f (int_of_float (lb *. float_of_int sx)) (size_x () / 2)
  | (None, Some ub) -> plot_func_bounds f (-1 * size_x () / 2) (int_of_float (ub *. float_of_int sx))
  | (Some lb, Some ub) -> plot_func_bounds f (int_of_float (lb *. float_of_int sx)) (int_of_float (ub *. float_of_int sx))

let plot_x_func_bounds f lb ub = 
  for i = lb to ub do
    let y = float_of_int (i) /. float_of_int sy in
    let x = int_of_float (f y *. (float_of_int sx)) in
    point (x + opx) (i + opy) 
  done

let plot_x_func f bounds = 
  match bounds with
  | (None, None) -> plot_x_func_bounds f (-1 * size_y () / 2) (size_y () / 2)
  | (Some lb, None) -> plot_x_func_bounds f (int_of_float (lb *. float_of_int sy)) (size_y () / 2)
  | (None, Some ub) -> plot_x_func_bounds f (-1 * size_y () / 2) (int_of_float (ub *. float_of_int sy))
  | (Some lb, Some ub) -> plot_x_func_bounds f (int_of_float (lb *. float_of_int sy)) (int_of_float (ub *. float_of_int sy))
  

let _ = axis opx opy

let w = [{Complex.re = 1.0;im = 3.0}; {Complex.re = 2.0;im = 1.0}; {Complex.re = -0.3;im = 1.2}; {Complex.re = 3.0;im = 3.0}; {Complex.re = 1.0;im = 3.0}; {Complex.re = 1.0;im = 3.0};] 
let rep = FreeParticleEvolutionEulers1D.from_list w
let domain = (-6., 6.)
let lengthdomain = (int_of_float) (snd domain -. fst domain)
let prob = FreeParticleEvolutionEulers1D.probabilities rep
let numPoints = List.length prob
let spaceBetween = (float) lengthdomain /. (float) numPoints

(* let _ = print_int numPoints; print_string " " ;print_float spaceBetween; print_string "\n" *)
;;
try
  while true do

    remember_mode false;
    let st = wait_next_event [Key_pressed] in
    synchronize ();
    let mx = st.mouse_x + 5 and my = st.mouse_y + 5 in
    set_color (rgb 0 0 0);

    let _ = List.mapi (fun i p ->
        let x = int_of_float ((fst domain +. spaceBetween *. (float) i) *. (float_of_int sx)) + opx in
        let y = opy in
        let h = int_of_float (p *. (float_of_int sy)) in
        let w = int_of_float (spaceBetween *. (float_of_int sx)) in
        let _ = fill_rect x y w h in
        p
      ) prob in

    set_color black;
  done
with Exit -> ()