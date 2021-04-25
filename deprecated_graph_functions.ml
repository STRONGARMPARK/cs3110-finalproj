(* let plot_func_bounds f lb ub = 
  for i = lb to ub do
    let x = float_of_int (i) /. float_of_int sx in
    let y = int_of_float (f x *. (float_of_int sy)) in
    point (i + opx) (y + opy) 
  done 
in

let plot_func f bounds = 
  match bounds with
  | (None, None) -> plot_func_bounds f (-1 * size_x () / 2) (size_x () / 2)
  | (Some lb, None) -> plot_func_bounds f (int_of_float (lb *. float_of_int sx)) (size_x () / 2)
  | (None, Some ub) -> plot_func_bounds f (-1 * size_x () / 2) (int_of_float (ub *. float_of_int sx))
  | (Some lb, Some ub) -> plot_func_bounds f (int_of_float (lb *. float_of_int sx)) (int_of_float (ub *. float_of_int sx))
in

let plot_x_func_bounds f lb ub = 
  for i = lb to ub do
    let y = float_of_int (i) /. float_of_int sy in
    let x = int_of_float (f y *. (float_of_int sx)) in
    point (x + opx) (i + opy) 
  done 
in

let plot_x_func f bounds = 
  match bounds with
  | (None, None) -> plot_x_func_bounds f (-1 * size_y () / 2) (size_y () / 2)
  | (Some lb, None) -> plot_x_func_bounds f (int_of_float (lb *. float_of_int sy)) (size_y () / 2)
  | (None, Some ub) -> plot_x_func_bounds f (-1 * size_y () / 2) (int_of_float (ub *. float_of_int sy))
  | (Some lb, Some ub) -> plot_x_func_bounds f (int_of_float (lb *. float_of_int sy)) (int_of_float (ub *. float_of_int sy)) 
in *)