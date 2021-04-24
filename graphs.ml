open Graphics
open Evolution1d
;;

let draw_text str x y = 
  let _ = moveto x y in
  draw_string str
let point x y = fill_circle x y 1
let point_size x y s = fill_circle x y s

let tick_h len opx opy sy y = 
  if y <> 0 then 
    let _ = draw_poly_line [|(opx - len / 2, opy + y * sy);(opx + len / 2, opy + y * sy)|] in
    draw_text (string_of_int y) (opx + len / 2 + 5) (opy + y * sy - 5)

let tick_v len opx opy sx x = 
  if x <> 0 then 
    let _ = draw_poly_line [|(opx + x * sx, opy - len / 2);(opx + x * sx, opy + len / 2)|] in
    draw_text (string_of_int x) (opx + x * sx) (opy - len / 2 - 15)

module type Graph = sig
  val graph : Evolution1d.domain -> Complex.t list -> string -> unit
end

module Make = 
functor (Solver : Evolution1D) -> struct 
  module S = Solver 
  let graph domain initial_condition boundary_condition = 
    
    let _ = open_graph ":0 720x720" in

    let sx = 40 in
    let sy = 320 in

    let l = [] in

    let _ = List.map (fun p -> point (fst p) (snd p)) l in

    let opx = (size_x () / 2) in

    let opy = (size_y () / 2) in

    let tick_length = 4 in

    let generate_ticks opx opy = 
      let _ = for i = 0 to size_x () / sx do tick_v tick_length opx opy sx (i - opx / sx) done in
      for i = 0 to size_y () / sy do tick_h tick_length opx opy sy (i - opy / sy) done 
    in

    let axis opx opy =
      let _ = draw_poly_line [|(0, opy);(size_x (), opy)|] in
      let _ = draw_poly_line [|(opx, 0);(opx, size_y ())|] in
      generate_ticks opx opy 
    in

    let _ = axis opx opy in

    let t = ref 0. in
    let t_elapsed = ref 0. in
    let w = ref initial_condition in 
    let domain = domain in
    let lengthdomain = (int_of_float) (snd domain -. fst domain) in

    try
      while true do
    
        remember_mode false;
        let st = wait_next_event [Key_pressed] in  
        synchronize ();
        (* let mx = st.mouse_x + 5 and my = st.mouse_y + 5 in *)
        set_color (rgb 0 0 0);

        let dt = 0.1 in
        t := Sys.time ();
        t_elapsed := !t_elapsed +. 0.1;
        let _ = draw_text ("Time: " ^ string_of_float !t_elapsed) (5) (size_y () - 15) in

        let rep = S.from_list !w in
        let prob = S.probabilities rep in
        w := 
        match boundary_condition with 
        | "periodic" ->  S.evolve rep 0.01 Periodic domain dt false |> S.to_list;
        | "dirichlet" -> S.evolve rep 0.01 Dirichlet domain dt false |> S.to_list;
        | _ -> failwith "not possible"; ;
      
        let numPoints = List.length prob in
        let spaceBetween = (float) lengthdomain /. (float) numPoints in
    
        let _ = List.mapi (fun i p ->
            let x = int_of_float ((fst domain +. spaceBetween *. (float) i) *. (float_of_int sx)) + opx in
            let y = opy in
            let h = int_of_float (p *. (float_of_int sy)) in
            let w = int_of_float (spaceBetween *. (float_of_int sx)) in
            let _ = fill_rect x y w h in
            (* let _ = draw_text (string_of_float p) x (y + h + 5) in *)
            p
          ) prob in
    
        set_color black;
      done
    with Exit -> ()

end 