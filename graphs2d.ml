open Graphics
open Evolution
;;

exception NotAMatrix of string

type m = float list list

type v = float list

type v3 = {x:float;y:float;z:float}

type box = {v1:v3;v2:v3;v3:v3;v4:v3;v5:v3;v6:v3;v7:v3;v8:v3}

let v3op_const op (b:float) (a:v3) : v3 = {x = op a.x b ; y = op a.y b ; z = op a.z b}
let v3op op (a:v3) (b:v3) : v3 = {x = op a.x b.x ; y = op a.y b.y ; z = op a.z b.z}

let to_float (i : int list list) : m = 
  List.map (List.map (fun a -> (float) a)) i

let v3_to_m (v3:v3) : m = [[v3.x];[v3.y];[v3.z]]
let m_to_v (m:m) : v = List.hd m
let v3_to_v (v3:v3) : v = [v3.x;v3.y;v3.z]
let v_to_v3 (v:v) : v3 = 
  match v with
  | [] -> {x=0.;y=0.;z=0.}
  | vx::[] -> {x=vx;y=0.;z=0.}
  | vx::vy::[] -> {x=vx;y=vy;z=0.}
  | vx::vy::vz::_ -> {x=vx;y=vy;z=vz}

let print_v (v:v) = 
  print_string "[ ";
  List.iter (fun x -> print_float x; print_string " ; ") v;
  print_string "]"; ()

let print_m (m:m) = 
  print_string "[\n";
  List.iter (fun v -> print_v v) m;
  print_string "\n]"; ()

let print_v3 (v3:v3) = 
  print_string "( ";
  print_float v3.x;
  print_string " , ";
  print_float v3.y;
  print_string " , ";
  print_float v3.z;
  print_string " )"; ()

let check (m : m) : m = 
  if (List.for_all (fun row -> List.length row = List.length (List.hd m)) m) 
    then m 
  else raise (NotAMatrix "Matrix does not have the same number of values in each row")

let rec transpose (m : m) : m = 
  match check m with 
  | [] -> []
  | []::s -> []
  | x -> List.map List.hd x :: transpose (List.map List.tl x)

let m_to_v3 (m:m) : v3 = v_to_v3 (List.hd (transpose m))

let ( ** ) (a : v) (b : v) : float = 
  List.fold_left2 (fun dot a b -> dot +. a *. b) 0. a b

let ( *** ) (a : m) (b : m) : m = 
  List.map (fun r -> List.map ( ( ** ) r ) ( transpose b )) a

let norm (p:v3) : v3 = 
  let mag = sqrt (p.x *. p.x +. p.y *. p.y +. p.z *. p.z) in
  v3op_const ( /. ) mag p

let box (a:v3) (b:v3) : box =
  {v1 = a; v2 = {a with x=b.x}; v3 = {a with x=b.x;y=b.y}; v4 = {a with y=b.y};
   v5 = {b with x=a.x;y=a.y}; v6 = {b with y=a.y}; v7 = b; v8 = {b with x=a.x}}

let box_map (f : v3 -> v3) (b : box) : box = 
  {v1=f b.v1; v2=f b.v2; v3=f b.v3; v4=f b.v4; v5=f b.v5; v6=f b.v6; v7=f b.v7; v8=f b.v8}

let draw_box (b : box) = 
  draw_poly_line [|(int_of_float b.v1.x,int_of_float b.v1.y);(int_of_float b.v2.x,int_of_float b.v2.y);
                  (int_of_float b.v3.x,int_of_float b.v3.y);(int_of_float b.v4.x,int_of_float b.v4.y);
                  (int_of_float b.v1.x,int_of_float b.v1.y)|];
  draw_poly_line [|(int_of_float b.v1.x,int_of_float b.v1.y);(int_of_float b.v2.x,int_of_float b.v2.y);
                  (int_of_float b.v6.x,int_of_float b.v6.y);(int_of_float b.v5.x,int_of_float b.v5.y);
                  (int_of_float b.v1.x,int_of_float b.v1.y)|];
  draw_poly_line [|(int_of_float b.v1.x,int_of_float b.v1.y);(int_of_float b.v4.x,int_of_float b.v4.y);
                  (int_of_float b.v8.x,int_of_float b.v8.y);(int_of_float b.v5.x,int_of_float b.v5.y);
                  (int_of_float b.v1.x,int_of_float b.v1.y)|];
  draw_poly_line [|(int_of_float b.v2.x,int_of_float b.v2.y);(int_of_float b.v3.x,int_of_float b.v3.y);
                  (int_of_float b.v7.x,int_of_float b.v7.y);(int_of_float b.v6.x,int_of_float b.v6.y);
                  (int_of_float b.v2.x,int_of_float b.v2.y)|];
  draw_poly_line [|(int_of_float b.v3.x,int_of_float b.v3.y);(int_of_float b.v4.x,int_of_float b.v4.y);
                  (int_of_float b.v8.x,int_of_float b.v8.y);(int_of_float b.v7.x,int_of_float b.v7.y);
                  (int_of_float b.v3.x,int_of_float b.v3.y)|];
  draw_poly_line [|(int_of_float b.v5.x,int_of_float b.v5.y);(int_of_float b.v6.x,int_of_float b.v6.y);
                  (int_of_float b.v7.x,int_of_float b.v7.y);(int_of_float b.v8.x,int_of_float b.v8.y);
                  (int_of_float b.v5.x,int_of_float b.v5.y)|];()

let rotateX (angle : float) : m = 
  let rad = angle *. pi /. 180. in
  [[1.;0.;0.];
  [0.;cos rad;-1. *. sin rad];
  [0.;sin rad;cos rad]]

let rotateY (angle : float) : m = 
  let rad = angle *. pi /. 180. in
  [[cos rad;0.;sin rad];
  [0.;1.;0.];
  [-1. *. sin rad;0.;cos rad]]

let rotateZ (angle : float) : m = 
  let rad = angle *. pi /. 180. in
  [[cos rad;-1. *. sin rad;0.];
  [sin rad;cos rad;0.];
  [0.;0.;1.]]


let camera_proj (co:v3) (wo:v3) (off:v3) (thetax:float) (thetay:float) (thetaz:float) (scale:float) (point:v3) : v3 = 
  let proj = [[1.;0.;0.];[0.;1.;0.]] in
  let diff = v3op (-.) wo co in
  let offset = v3op (-.) point diff in
  let rotated = m_to_v3 (rotateZ thetaz *** rotateX thetax *** rotateY thetay *** (v3_to_m offset)) in
  let point = rotated |> v3op ( +. ) diff |> v3op_const ( *. ) scale |> v3op ( +. ) off  in
  m_to_v3 (proj *** (v3_to_m point))

let tick proj (len:float) (p:v3) (dir:v3) : unit = 
  let p1 = proj (v3op_const ( *. ) (len /. -2.) dir |> v3op ( +. ) p) in
  let p2 = proj (v3op_const ( *. ) (len /. 2.) dir |> v3op ( +. ) p) in
  draw_poly_line [|(int_of_float p1.x,int_of_float p1.y);(int_of_float p2.x,int_of_float p2.y)|];
  ()

let axis proj (dir:v3) (len:float) : unit = 
  let p1 = proj (v3op_const ( *. ) (len /. -2.) dir) in 
  let p2 = proj (v3op_const ( *. ) ((len /. 2.)) dir) in
  draw_poly_line [|(int_of_float p1.x,int_of_float p1.y);(int_of_float p2.x,int_of_float p2.y)|];()
  
let all_axis proj len sx sy sz : unit =
  axis proj {x=1.;y=0.;z=0.} len;
  axis proj {x=0.;y=1.;z=0.} len;
  axis proj {x=0.;y=0.;z=1.} len;
  for i= int_of_float (-1. *. len /. 2. /.sx) to int_of_float (len /. 2. /.sx) do 
    tick proj 0.7 {x=(float)i*.sx;y=0.;z=0.} {x=0.;y=0.;z=1.};
  done;
  for i= int_of_float (-1. *. len /. 2. /.sy) to int_of_float (len /. 2. /.sy) do 
    tick proj 0.7 {x=0.;y=(float)i*.sy;z=0.} {x=1.;y=0.;z=0.};
  done;
  for i= int_of_float (-1. *. len /. 2. /.sz) to int_of_float (len /. 2. /.sz) do 
    tick proj 0.7 {x=0.;y=0.;z=(float)i*.sz} {x=1.;y=0.;z=0.};
  done;
  ()


module type Graph = sig
  val graph_prob : Evolution.domain2d -> (Complex.t list) list -> Evolution.boundary_conditions list -> unit
  val graph_wave : Evolution.domain2d -> (Complex.t list) list -> Evolution.boundary_conditions list -> unit
end

module Make = 
functor (Solver : Evolution2D) -> struct 
  module S = Solver 

  let graph_prob (domain2d : Evolution.domain2d) 
                  (initial_condition : (Complex.t list) list) 
                  (boundary_condition : Evolution.boundary_conditions list) = 
    let _ = open_graph ":0 700x700" in
    let angle = ref 0. in
    let scale = ref 10. in
    let cam = {x = 0. ; y = 0. ; z = 0.} in
    let wo = {x = 0. ; y = 0. ; z = 0.} in 
    let off = {x = 350. ; y = 250. ; z = 100.} in
    let proj = ref (camera_proj cam wo off 25. (-45. +. !angle) 0. !scale) in
    let _ = all_axis !proj 500. 1. 1. 1. in
    try
      while true do
        let key = read_key () in  
        synchronize ();
        set_color (rgb 0 0 0);
        let _ = remember_mode true in 
        let _ = (match key with 
        | 'a' -> (angle := !angle +. -3.; 
                  clear_graph (); 
                  proj := camera_proj cam wo off 25. (-45. +. !angle) 0. !scale;
                  all_axis !proj 500. 1. 1. 1.;
                  )
        | 'd' -> (angle := !angle +. 3.; 
                  clear_graph (); 
                  proj := camera_proj cam wo off 25. (-45. +. !angle) 0. !scale;
                  all_axis !proj 500. 1. 1. 1.;
                  )
        | 'w' -> (scale := !scale +. 0.5;
                  clear_graph (); 
                  proj := camera_proj cam wo off 25. (-45. +. !angle) 0. !scale;
                  all_axis !proj 500. 1. 1. 1.;
                  )
        | 's' -> (scale := !scale -. 0.5;
                  clear_graph (); 
                  proj := camera_proj cam wo off 25. (-45. +. !angle) 0. !scale;
                  all_axis !proj 500. 1. 1. 1.;
                  )
        | _ -> ()) in
        let _ = remember_mode false in
        let b = box {x=10.;y=10.;z=10.} {x= 0.;y= 0.;z= 0.} in
        let b = box_map (fun v3 -> !proj v3) b in
        draw_box b;
        set_color black;
      done
    with Exit -> ()


  let graph_wave (domain2d : Evolution.domain2d) 
                  (initial_condition : (Complex.t list) list) 
                  (boundary_condition : Evolution.boundary_conditions list) = 
    failwith "not implemented"

end