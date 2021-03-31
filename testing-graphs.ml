
open Graphics
;;
let _ = open_graph " 600x400"

let sx = 10 

let sy = 10 

let point x y =
  let _ = set_color black in 
  fill_circle x y 1

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

let plot_func f = 
  for i = 0 to size_x () do 
    let x = float_of_int (i - opx) /. float_of_int sx in
    let y = int_of_float (f x *. (float_of_int sy)) in
    point i (y + opy) 
  done

let _ = axis opx opy

let _ = plot_func (fun x -> sin (x))

let _ = wait_next_event [ Key_pressed ]