open Graphics
open Evolution

(** [vop op a b] takes 2d vertex [a] and 2d vertex [b] and applies
    operation [op] to them *)
let vop op a b = (op (fst a) (fst b), op (snd a) (snd b))

(** [vfloat a] is the 2d float vertex from 2d int vertex [a] *)
let vfloat (a : int * int) = (float (fst a), float (snd a))

(** [vint a] is the 2d int vertex from 2d float vertex [a] *)
let vint (a : float * float) =
  (int_of_float (fst a), int_of_float (snd a))

(** [mag_float a] is the magnitude of 2d float vertex [a] *)
let mag_float a = sqrt ((fst a *. fst a) +. (snd a *. snd a))

(** [mag_int a] is the magnitude of 2d int vertex [a] *)
let mag_int a = mag_float (vfloat a)

(** [draw_text str x y] draws text [str] at position [x], [y] *)
let draw_text str x y =
  let _ = moveto x y in
  draw_string str

(** [point x y] draws point at position [x], [y] *)
let point x y = fill_circle x y 1

(** [point_size x y s] draws point with size [s] at position [x], [y] *)
let point_size x y s = fill_circle x y s

(** [tick_h] draws a horizontal tick mark of length [len] at position
    [y] given the origin position [opx] and [opy] and the y-axis scaling
    factor [sy] *)
let tick_h len opx opy sy y =
  if y <> 0 then
    let _ =
      draw_poly_line
        [|
          (opx - (len / 2), opy + (y * sy));
          (opx + (len / 2), opy + (y * sy));
        |]
    in
    draw_text (string_of_int y)
      (opx + (len / 2) + 5)
      (opy + (y * sy) - 5)

(** [tick_v] draws a vertical tick mark of length [len] at position [x]
    given the origin position [opx] and [opy] and the x-axis scaling
    factor [sx] *)
let tick_v len opx opy sx x =
  if x <> 0 then
    let _ =
      draw_poly_line
        [|
          (opx + (x * sx), opy - (len / 2));
          (opx + (x * sx), opy + (len / 2));
        |]
    in
    draw_text (string_of_int x) (opx + (x * sx)) (opy - (len / 2) - 15)

(** [dotted_line_section size space p2 cur] draws a line at point [p2]
    with size [size] in the direction of [cur] - [p2]. *)
let rec dotted_line_section size space p2 cur =
  let dir = vop ( -. ) (vfloat p2) (vfloat cur) in
  let m = mag_float dir in
  let dist_to_next =
    vop ( /. ) dir (m, m) |> vop ( *. ) (vfloat (size, size))
  in
  let next = vop ( + ) cur (vint dist_to_next) in
  if mag_int (vop ( - ) p2 next) > 0. then
    let _ = draw_poly_line [| cur; next |] in
    let dist_to_next =
      vop ( /. ) dir (m, m)
      |> vop ( *. ) (vfloat (size + space, size + space))
    in
    let next = vop ( + ) cur (vint dist_to_next) in
    dotted_line_section size space p2 next
  else ""

(** [dotted_line size space p1 p2] draws a dotted line from position
    [p1] to [p2] with tick size of [size] with space of [space] between
    them *)
let dotted_line size space p1 p2 = dotted_line_section size space p2 p1

module type Graph = sig
  val graph_prob :
    Evolution.domain ->
    Complex.t list ->
    Evolution.boundary_conditions ->
    unit
end

module Make =
functor
  (Solver : Evolution1D)
  ->
  struct
    module S = Solver

    (** [setup_graph w h opx opy sx sy len] opens an ocaml graphics
        window with sizes [w]x[h] and draws axis with tick marks of
        length [len] at origin [opx],[opy] with scaling factors [sx] and
        [sy] for x and y axis respectively.*)
    let setup_graph w h opx opy sx sy tick_length =
      let _ =
        open_graph (":0 " ^ string_of_int w ^ "x" ^ string_of_int h)
      in
      let generate_ticks opx opy =
        let _ =
          for i = 0 to size_x () / sx do
            tick_v tick_length opx opy sx (i - (opx / sx))
          done
        in
        for i = 0 to size_y () / sy do
          tick_h tick_length opx opy sy (i - (opy / sy))
        done
      in
      let axis opx opy =
        let _ = draw_poly_line [| (0, opy); (size_x (), opy) |] in
        let _ = draw_poly_line [| (opx, 0); (opx, size_y ()) |] in
        generate_ticks opx opy
      in
      axis opx opy

    (** [graph_prob d t b] graphs the probability distribution for
        particles in a one dimensional domain given a domain [d] a list
        of initial conditions as complex numbers [t] in a boundary
        condition [b] for the solver to use *)
    let graph_prob domain initial_condition boundary_condition =
      let domain = domain in
      let lengthdomain = int_of_float (snd domain -. fst domain) in

      let width = 700 in
      let height = 700 in
      let opx = 350 in
      let opy = 350 in
      let sx = int_of_float (float (width / lengthdomain) *. 0.9) in
      let sy = 320 in
      let _ = setup_graph width height opx opy sx sy 4 in
      let _ = draw_text "x" (size_x () - 15) (opy - 15) in
      let _ = draw_text "y" (opx + 10) (size_y () - 15) in

      let t = ref 0. in
      let t_elapsed = ref 0. in
      let w = ref initial_condition in

      try
        while true do
          remember_mode false;
          let _ = wait_next_event [ Key_pressed ] in
          synchronize ();
          set_color (rgb 0 0 0);

          let deltat = 0.1 in
          t := Sys.time ();
          t_elapsed := !t_elapsed +. deltat;
          let _ =
            draw_text
              ("Time: " ^ string_of_float !t_elapsed)
              5
              (size_y () - 15)
          in

          let rep = S.from_list !w in
          let prob = S.probabilities rep in
          w :=
            S.evolve rep 0.08 boundary_condition domain 0.2 false
            |> S.to_list;

          let numPoints = List.length prob in
          let spaceBetween = float lengthdomain /. float numPoints in

          let cur_p = ref 0. in

          let _ =
            List.mapi
              (fun i p ->
                if p > !cur_p then cur_p := p;
                let x =
                  int_of_float
                    ((fst domain +. (spaceBetween *. float i))
                    *. float_of_int sx)
                  + opx
                in
                let y = opy in
                let h = int_of_float (p *. float_of_int sy) in
                let w =
                  int_of_float (spaceBetween *. float_of_int sx)
                in
                let gray =
                  max
                    (int_of_float (250. *. float i /. float numPoints))
                    0
                in
                let c = rgb gray gray gray in
                let _ = set_color c in
                let _ = fill_rect x y w h in
                (* let _ = draw_text (string_of_float p) x (y + h + 5)
                   in *)
                p)
              prob
          in

          let max_p = opy + int_of_float (!cur_p *. float sy) in
          let _ = set_color black in
          let _ = dotted_line 5 10 (0, max_p) (size_x (), max_p) in
          let _ = draw_text (string_of_float !cur_p) 10 (max_p + 10) in

          set_color black
        done
      with Exit -> ()
  end
