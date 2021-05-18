open Graphics
open Evolution
;;

exception NotAMatrix of string

type m = float list list

type v = float list

type v3 = {x:float;y:float;z:float}

type camera = {o:v3;s:float;rx:float;ry:float;rz:float}

type box = {v1:v3;v2:v3;v3:v3;v4:v3;v5:v3;v6:v3;v7:v3;v8:v3}

let v3op_const op (b:float) (a:v3) : v3 = 
  {x = op a.x b ; y = op a.y b ; z = op a.z b}
let v3op op (a:v3) (b:v3) : v3 = 
  {x = op a.x b.x ; y = op a.y b.y ; z = op a.z b.z}
let v3op_bool op (a:v3) (b:v3) : bool = 
  (op a.x b.x) && (op a.y b.y) && (op a.z b.z)
let v3op_bool_const op (b:float) (a:v3) : bool = 
  (op a.x b) && (op a.y b) && (op a.z b)

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

let dot (a : v3) (b : v3) : float = a.x*.b.x+.a.y*.b.y+.a.z*.b.z
let cross (a : v3) (b : v3) : v3 = 
  {x=a.y*.b.z-.a.z*.b.y ; y=a.z*.b.x-.a.x*.b.z ; z=a.x*.b.y-.a.y*.b.x}

let ( ** ) (a : v) (b : v) : float = 
  List.fold_left2 (fun dot a b -> dot +. a *. b) 0. a b

let ( *** ) (a : m) (b : m) : m = 
  List.map (fun r -> List.map ( ( ** ) r ) ( transpose b )) a

let mag (p:v3) : float = sqrt (dot p p)
let norm (p:v3) : v3 = 
  v3op_const ( /. ) (mag p) p

let camera_pos cam =
  v3op_const ( *. ) (1400. /. cam.s) {x= -1. *. sin (cam.ry *. pi /. 180.);y=sin (cam.rx *. pi /. 180.);z=cos (cam.ry *. pi /. 180.)}

let box (a:v3) (b:v3) : box =
  let dif = v3op (-.) a b in
  let ac = a in let bc = b in
  let a = {x = if dif.x > 0. then bc.x else ac.x; y = if dif.y > 0. then bc.y else ac.y; z = if dif.z > 0. then bc.z else ac.z;} in
  let b = {x = if dif.x > 0. then ac.x else bc.x; y = if dif.y > 0. then ac.y else bc.y; z = if dif.z > 0. then ac.z else bc.z;} in
  {v1 = a; v2 = {a with x=b.x}; v3 = {a with x=b.x;z=b.z}; v4 = {a with z=b.z};
   v5 = {b with x=a.x;z=a.z}; v6 = {b with z=a.z}; v7 = b; v8 = {b with x=a.x}}

let box_map (f : v3 -> v3) (b : box) : box = 
  {v1=f b.v1;v2=f b.v2;v3=f b.v3;v4=f b.v4;v5=f b.v5;v6=f b.v6;v7=f b.v7;v8=f b.v8}

let box_fold_left (f : 'a -> v3 -> 'a) (init : 'a) (b : box) : 'a =
  (f (f (f (f (f (f (f (f init b.v1) b.v2) b.v3) b.v4) b.v5) b.v6) b.v7) b.v8)

let draw_side v1 v2 v3 v4 cp proj = 
  let n = cross (v3op ( -. ) v1 v2) (v3op ( -. ) v4 v1) in
  let d = dot (v3op ( -. ) v1 cp) n in
  if d >= 0. then (
    let v1 = proj v1 in let v2 = proj v2 in let v3 = proj v3 in let v4 = proj v4 in
    fill_poly [|(int_of_float v1.x,int_of_float v1.y);(int_of_float v2.x,int_of_float v2.y);
      (int_of_float v3.x,int_of_float v3.y);(int_of_float v4.x,int_of_float v4.y);
      (int_of_float v1.x,int_of_float v1.y)|]);()

let draw_box cam proj (b : box) = 
  let cp = camera_pos cam in
  set_color black;
  draw_side b.v1 b.v2 b.v3 b.v4 cp proj;
  set_color (rgb 150 150 150);
  draw_side b.v5 b.v6 b.v2 b.v1 cp proj;
  set_color (rgb 100 100 100);
  draw_side b.v1 b.v4 b.v8 b.v5 cp proj;
  set_color (rgb 100 100 100);
  draw_side b.v6 b.v7 b.v3 b.v2 cp proj;
  set_color (rgb 150 150 150);
  draw_side b.v7 b.v8 b.v4 b.v3 cp proj;
  set_color (rgb 200 200 200);
  draw_side b.v8 b.v7 b.v6 b.v5 cp proj;
  ()

let box_closest_v3 (point:v3) (b:box) : v3 = 
  box_fold_left 
  (fun a b -> let pa = v3op (-.) a point in let pb = v3op (-.) b point in
    if pa.x *. pa.x +. pa.y *. pa.y > pb.x *. pb.x +. pb.y *. pb.y then a else b) 
  point b

let rec separate (boxes: box list) (cp : v3) : box list * box list = 
  let b1 = List.filter (fun b -> print_v3 (box_closest_v3 cp b); print_string "\n"; dot {cp with z=0.} {(box_closest_v3 cp b) with z=0.} >= 0.) boxes in
  let b2 = List.filter (fun b -> dot {cp with z=0.;} {(box_closest_v3 cp b) with z=0.;} < 0.) boxes in
  (b1, b2)


let sort_boxes cam b =
  let cp = camera_pos cam in
  let b = List.sort (
      fun b1 b2 ->
        let min1 = v3op (-.) cp (box_closest_v3 cp b1) in
        let min2 = v3op (-.) cp (box_closest_v3 cp b2) in
        if mag min1 < mag min2 then 1
        else if mag min1 = mag min2 then 0
        else -1
    ) b in
  separate b cp

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

let camera_proj (cam:camera) (point:v3) : v3 = 
  let proj = [[1.;0.;0.];[0.;1.;0.]] in
  let rotated = m_to_v3 (rotateZ cam.rz *** rotateX cam.rx *** rotateY cam.ry *** (v3_to_m point)) in
  let point = rotated |> v3op_const ( *. ) cam.s |> v3op ( +. ) cam.o  in
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
  
let xz_axis proj len sx sy sz : unit =
  axis proj {x=1.;y=0.;z=0.} len;
  axis proj {x=0.;y=0.;z=1.} len;
  for i= int_of_float (-1. *. len /. 2. /.sx) to int_of_float (len /. 2. /.sx) do 
    tick proj 0.7 {x=(float)i*.sx;y=0.;z=0.} {x=0.;y=0.;z=1.};
  done;
  for i= int_of_float (-1. *. len /. 2. /.sz) to int_of_float (len /. 2. /.sz) do 
    tick proj 0.7 {x=0.;y=0.;z=(float)i*.sz} {x=1.;y=0.;z=0.};
  done;
  ()

let y_axis proj len sx sy sz : unit =
  axis proj {x=0.;y=1.;z=0.} len;
  for i= int_of_float (-1. *. len /. 2. /.sy) to int_of_float (len /. 2. /.sy) do 
    tick proj 0.7 {x=0.;y=(float)i*.sy;z=0.} {x=1.;y=0.;z=0.};
  done;
  ()

let updatecam cam proj key = 
  match key with 
  | 'a' -> (cam := {!cam with ry = !cam.ry -. 1.};
            proj := camera_proj !cam;)
  | 'd' -> (cam := {!cam with ry = !cam.ry +. 1.};
            proj := camera_proj !cam;)
  | 'w' -> (cam := {!cam with s = !cam.s *. 1.1};
            proj := camera_proj !cam;)
  | 's' -> (cam := {!cam with s = !cam.s *. 0.9};
            proj := camera_proj !cam;)
  | 'r' -> (cam := {!cam with s = 10.;ry= -45.};
            proj := camera_proj !cam;)
  | _ -> ()

let draw_xz_axis cam proj key = 
  let _ = set_color black in
  match key with 
  | 'a' -> (remember_mode true;
            clear_graph ();
            xz_axis !proj 500. 1. 1. 1.;
            remember_mode false)
  | 'd' -> (remember_mode true;
            clear_graph ();
            xz_axis !proj 500. 1. 1. 1.;
            remember_mode false;)
  | 'w' -> (remember_mode true;
            clear_graph ();
            xz_axis !proj 500. 1. 1. 1.;
            remember_mode false;)
  | 's' -> (remember_mode true;
            clear_graph ();
            xz_axis !proj 500. 1. 1. 1.;
            remember_mode false;)
  | 'r' -> (remember_mode true;
            clear_graph ();
            xz_axis !proj 500. 1. 1. 1.;
            remember_mode false;)
  | _ -> ()
  
let draw_y_axis cam proj key = 
  let _ = set_color black in
  y_axis !proj 500. 1. 1. 1.

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
    let cam = ref {o={x = 350. ; y = 250. ; z = 0.};s=10.;rx=25.;ry= -45.;rz=0.} in
    let proj = ref (camera_proj !cam) in
    let _ = xz_axis !proj 500. 1. 1. 1. in
    let _ = y_axis !proj 500. 1. 1. 1. in
    try
      while true do
        let key = read_key () in  
        synchronize ();
        set_color (rgb 0 0 0);
        updatecam cam proj key;
        let boxes = [box {x=0.;y=0.;z=0.} {x=10.;y=10.;z=10.};
                    box {x=0.;y=0.;z=0.} {x= -10.;y=15.;z=10.};
                    box {x=0.;y=0.;z=0.} {x=10.;y=5.;z= -10.};
                    box {x=0.;y=0.;z=0.} {x= -10.;y=20.;z= -10.};] in
        let boxes = sort_boxes !cam boxes in
        let _ = draw_xz_axis cam proj key in
        let _ = List.iter (fun b -> draw_box !cam !proj b) (snd boxes) in
        let _ = draw_y_axis cam proj key in
        (* let _ = List.iter (fun b -> draw_box !cam !proj b) (fst boxes) in *)
        set_color black;
      done
    with Exit -> ()


  let graph_wave (domain2d : Evolution.domain2d) 
                  (initial_condition : (Complex.t list) list) 
                  (boundary_condition : Evolution.boundary_conditions list) = 
    failwith "not implemented"

end