open Graphics
open Evolution1d
;;

module type Graph = sig
  val graph : 'a -> unit
end

module Make = 
functor (Solver : Evolution1D) -> struct 
  module S = Solver 
  let graph x = 
    
    let _ = open_graph ":0 720x720" in
    let sx = 40 in
    let sy = 320 in

    let point x y = fill_circle x y 1 in
    let point_size x y s = fill_circle x y s in

    let l = [] in

    let _ = List.map (fun p -> point (fst p) (snd p)) l in

    let opx = (size_x () / 2) in

    let opy = (size_y () / 2) in

    let tick_length = 4 in

    let tick_h opx opy y = 
      draw_poly_line [|(opx - tick_length / 2, opy + y * sy); 
                      (opx + tick_length / 2, opy + y * sy)|] 
    in

    let tick_v opx opy x = 
      draw_poly_line [|(opx + x * sx, opy - tick_length / 2); 
                      (opx + x * sx, opy + tick_length / 2)|] 
    in

    let generate_ticks opx opy = 
      let _ = for i = 0 to size_x () / sx do tick_v opx opy (i - opx / sx) done in
      for i = 0 to size_y () / sy do tick_h opx opy (i - opy / sy) done 
    in

    let axis opx opy =
      let _ = draw_poly_line [|(0, opy);(size_x (), opy)|] in
      let _ = draw_poly_line [|(opx, 0);(opx, size_y ())|] in
      generate_ticks opx opy 
    in

    let _ = axis opx opy in

    let time = ref 0;
    let w = ref [{Complex.re = 1.0;im = 3.0}; {Complex.re = 2.0;im = 1.0}; {Complex.re = -0.3;im = 1.2}; {Complex.re = 3.0;im = 3.0}; {Complex.re = 1.0;im = 3.0}; {Complex.re = 1.0;im = 3.0};] in 
    let rep = S.from_list !w in
    let domain = (-6., 6.) in
    let lengthdomain = (int_of_float) (snd domain -. fst domain) in

    try
      while true do
    
        remember_mode false;
        let st = wait_next_event [Key_pressed] in
        synchronize ();
        (* let mx = st.mouse_x + 5 and my = st.mouse_y + 5 in *)
        set_color (rgb 0 0 0);

        (* time := Sys.time () - !time in *)
        let prob = S.probabilities rep in
        let numPoints = List.length prob in
        let spaceBetween = (float) lengthdomain /. (float) numPoints in
    
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

end 