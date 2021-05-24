open Graphics
open Evolution

exception NotAMatrix of string

(** [m] is a 2D list of floats to create a mathematical matrix *)
type m = float list list

(** [v] is a list of floats to create a mathematical vertex *)
type v = float list

(** [v3] is a vector with three points representing the three dimensions *)
type v3 = {
  x : float;
  y : float;
  z : float;
}

(** [camera] contains the position [o], the scale [s], the rotation in x
    [rx], rotation in y [ry] and rotation in z [rz]*)
type camera = {
  o : v3;
  s : float;
  rx : float;
  ry : float;
  rz : float;
}

(** [box] contains vertices for all six points in a box *)
type box = {
  v1 : v3;
  v2 : v3;
  v3 : v3;
  v4 : v3;
  v5 : v3;
  v6 : v3;
  v7 : v3;
  v8 : v3;
}

(** [c] is a color with three float values for red, green and blue *)
type c = {
  r : float;
  g : float;
  b : float;
}

(** [int_to_c i] take an integer [i] and returns a color type [c] with
    red, green and blue values *)
let int_to_c c =
  let b = c mod 256 in
  let g = (c - b) / 256 mod 256 in
  let r = ((c - b) / (256 * 256)) - (g / 256) in
  { r = float r; g = float g; b = float b }

(** [v3pop_const op b a] takes an float operation [op] and applies it to
    3D vertex [a] with constant [b]. Applies operator to each vertex
    value seperately. Order of operations is a [op] b. ex. a +. b *)
let v3op_const op (b : float) (a : v3) : v3 =
  { x = op a.x b; y = op a.y b; z = op a.z b }

(** [v3pop op b a] takes an float operation [op] and applies it to 3D
    vertex [a] and 3d vertex [b]. Applies operator to each vertex value
    seperately. Order of operations is a [op] b. ex. a \+. b *)
let v3op op (a : v3) (b : v3) : v3 =
  { x = op a.x b.x; y = op a.y b.y; z = op a.z b.z }

(** [v3pop_bool op b a] takes an boolean operator [op] and applies it to
    3D vertex [a] and 3d vertex [b]. Applies operator to each vertex
    value seperately. Order of operations is a [op] b. ex. a = b *)
let v3op_bool op (a : v3) (b : v3) : bool =
  op a.x b.x && op a.y b.y && op a.z b.z

(** [v3pop_bool_const op b a] takes an boolean operator [op] and applies
    it to 3D vertex [a] and float [b]. Applies operator to each vertex
    value seperately. Order of operations is a [op] b. ex. a = b *)
let v3op_bool_const op (b : float) (a : v3) : bool =
  op a.x b && op a.y b && op a.z b

(** [box_bool op a b] takes an boolean operator [op] and applies it to
    box [a] and box [b]. Applies operator to each vertex value
    seperately. Order of operations is a [op] b. ex. a = b *)
let box_bool op (a : box) (b : box) : bool =
  op a.v1 b.v1 && op a.v2 b.v2 && op a.v3 b.v3 && op a.v4 b.v4
  && op a.v5 b.v5 && op a.v6 b.v6

(** [to_float i] takes a 2D list of integers and converts it into a 2D
    list of floats with type [m] *)
let to_float (i : int list list) : m =
  List.map (List.map (fun a -> float a)) i

(** [v3_to_m v3] takes a 3D vertex [v3] and transforms it into a matrix
    [m] *)
let v3_to_m (v3 : v3) : m = [ [ v3.x ]; [ v3.y ]; [ v3.z ] ]

(** [m_to_v m] takes a matrix [m] and transforms it into a vertex [v].
    If matrix [m] has more rows than 1, it only uses the first row *)
let m_to_v (m : m) : v = List.hd m

(** [v3_to_v v3] takes a 3D vertex [v3] and transforms it into a vertex
    [v]. *)
let v3_to_v (v3 : v3) : v = [ v3.x; v3.y; v3.z ]

(** [v_to_v3 v] takes an N dimension vertex [v] and transforms it into a
    3d vertex [v3] by getting the first 3 values, substituting 0 for all
    unknown values. *)
let v_to_v3 (v : v) : v3 =
  match v with
  | [] -> { x = 0.; y = 0.; z = 0. }
  | [ vx ] -> { x = vx; y = 0.; z = 0. }
  | [ vx; vy ] -> { x = vx; y = vy; z = 0. }
  | vx :: vy :: vz :: _ -> { x = vx; y = vy; z = vz }

(** [print_v a] pretty prints [a] of type [v] *)
let print_v (v : v) =
  print_string "[ ";
  List.iter
    (fun x ->
      print_float x;
      print_string " ; ")
    v;
  print_string "]";
  ()

(** [print_m a] pretty prints [a] of type [m] *)
let print_m (m : m) =
  print_string "[\n";
  List.iter (fun v -> print_v v) m;
  print_string "\n]";
  ()

(** [print_v3 a] pretty prints [a] of type [v3] *)
let print_v3 (v3 : v3) =
  print_string "( ";
  print_float v3.x;
  print_string " , ";
  print_float v3.y;
  print_string " , ";
  print_float v3.z;
  print_string " )";
  ()

(** [print_c a] pretty prints [a] of type [c] *)
let print_c (c : c) =
  print_string "( r = ";
  print_float c.r;
  print_string " , g = ";
  print_float c.g;
  print_string " , b = ";
  print_float c.b;
  print_string " )";
  ()

(** [print_box a] pretty prints [a] of type [box] *)
let print_box (b : box) =
  print_string "{ ";
  print_v3 b.v1;
  print_string " , ";
  print_v3 b.v2;
  print_string " , ";
  print_v3 b.v3;
  print_string " , ";
  print_v3 b.v4;
  print_string " , ";
  print_v3 b.v5;
  print_string " , ";
  print_v3 b.v6;
  print_string " , ";
  print_v3 b.v7;
  print_string " , ";
  print_v3 b.v8;
  print_string " }";
  ()

(** [check m] checks if m is a valid matrix. A valid matrix is
    rectangle, so has the same number of columns for each row, or the
    same number of rows for each column. *)

let check (m : m) : m =
  if
    List.for_all
      (fun row -> List.length row = List.length (List.hd m))
      m
  then m
  else
    raise
      (NotAMatrix
         "Matrix does not have the same number of values in each row")

(** [transpose m] transposes matrix [m]. A transposed matrix of size ...
    (n x m) has size (m x n)*)
let rec transpose (m : m) : m =
  match check m with
  | [] -> []
  | [] :: s -> []
  | x -> List.map List.hd x :: transpose (List.map List.tl x)

(** [m_to_v3 m] turns matrix [m] into a 3d vector of type [v3] *)
let m_to_v3 (m : m) : v3 = v_to_v3 (List.hd (transpose m))

(** [dot a b] applies a dot product operation to 3D vertices a and b.
    Order of operations is a dot b *)
let dot (a : v3) (b : v3) : float =
  (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)

(** [cross a b] applies a cross product operation to 3D vertices a and
    b. Order of operations is a cross b *)
let cross (a : v3) (b : v3) : v3 =
  {
    x = (a.y *. b.z) -. (a.z *. b.y);
    y = (a.z *. b.x) -. (a.x *. b.z);
    z = (a.x *. b.y) -. (a.y *. b.x);
  }

(** [(**) a b] is the dot product of two vertices of N dimensions with
    type [v] *)
let ( ** ) (a : v) (b : v) : float =
  List.fold_left2 (fun dot a b -> dot +. (a *. b)) 0. a b

(** [(***) a b] multiplies matrix [a] by matrix [b] *)
let ( *** ) (a : m) (b : m) : m =
  List.map (fun r -> List.map (( ** ) r) (transpose b)) a

(** [mag p] is the magnitude of 3D vector [p] *)
let mag (p : v3) : float = sqrt (dot p p)

(** [norm p] is the normalized 3D vector of 3D vector [p] *)
let norm (p : v3) : v3 = v3op_const ( /. ) (mag p) p

(** [camera_pos cam] is the position of the camera of type [v3] given
    [cam] of type [camera] *)
let camera_pos cam =
  v3op_const ( *. ) (5000. /. cam.s)
    {
      x = -1. *. sin (cam.ry *. pi /. 180.);
      y = sin (cam.rx *. pi /. 180.);
      z = cos (cam.ry *. pi /. 180.);
    }

(** [draw_point proj p size] takes a projection function [proj] a 3D
    point [p] and the size of the drawn point [size] and draws it on the
    canvas *)
let draw_point proj (p : v3) (size : int) : unit =
  let p = proj p in
  fill_circle (int_of_float p.x) (int_of_float p.y) size

(** [box a b] is the [box] type with 3D vectors [a] and [b] as two
    opposite corners of the box.*)
let box (a : v3) (b : v3) : box =
  let dif = v3op ( -. ) a b in
  let ac = a in
  let bc = b in
  let a =
    {
      x = (if dif.x > 0. then bc.x else ac.x);
      y = (if dif.y > 0. then bc.y else ac.y);
      z = (if dif.z > 0. then bc.z else ac.z);
    }
  in
  let b =
    {
      x = (if dif.x > 0. then ac.x else bc.x);
      y = (if dif.y > 0. then ac.y else bc.y);
      z = (if dif.z > 0. then ac.z else bc.z);
    }
  in
  {
    v1 = a;
    v2 = { a with x = b.x };
    v3 = { a with x = b.x; z = b.z };
    v4 = { a with z = b.z };
    v5 = { b with x = a.x; z = a.z };
    v6 = { b with z = a.z };
    v7 = b;
    v8 = { b with x = a.x };
  }

(** [box_map f b] applies function [f] to each vertex in box [b]
    seperately *)
let box_map (f : v3 -> v3) (b : box) : box =
  {
    v1 = f b.v1;
    v2 = f b.v2;
    v3 = f b.v3;
    v4 = f b.v4;
    v5 = f b.v5;
    v6 = f b.v6;
    v7 = f b.v7;
    v8 = f b.v8;
  }

(** [box_fold_left f init b] is f (... (f (f init v1) v2) ...) v8] where
    v1...v8 are vertices in box [b] *)
let box_fold_left (f : 'a -> v3 -> 'a) (init : 'a) (b : box) : 'a =
  f
    (f (f (f (f (f (f (f init b.v1) b.v2) b.v3) b.v4) b.v5) b.v6) b.v7)
    b.v8

(** [draw_side v1 v2 v3 v4 cp proj] draws a polygon with vertices
    [v1],[v2],[v3],[v4] on canvas with projection function [proj] if
    camera with position [cp] can see the shape. This is called face
    culling in 3D rendering practice. *)
let draw_side v1 v2 v3 v4 cp proj =
  let n = cross (v3op ( -. ) v1 v2) (v3op ( -. ) v4 v1) in
  let d = dot (v3op ( -. ) v1 cp) n in
  (if d >= 0. then
   let v1 = proj v1 in
   let v2 = proj v2 in
   let v3 = proj v3 in
   let v4 = proj v4 in
   fill_poly
     [|
       (int_of_float v1.x, int_of_float v1.y);
       (int_of_float v2.x, int_of_float v2.y);
       (int_of_float v3.x, int_of_float v3.y);
       (int_of_float v4.x, int_of_float v4.y);
       (int_of_float v1.x, int_of_float v1.y);
     |]);
  ()

(** [draw_box cam proj b c] draws box [b] with projection function
    [proj] on a 2d canvas in color [c] using camera [cam] for culling
    operations *)
let draw_box cam proj (b : box) c =
  let cp = camera_pos cam in
  let c1 =
    rgb
      (int_of_float (c.r *. 0.75))
      (int_of_float (c.g *. 0.75))
      (int_of_float (c.b *. 0.75))
  in
  let c2 =
    rgb
      (int_of_float (c.r *. 0.5))
      (int_of_float (c.g *. 0.5))
      (int_of_float (c.b *. 0.5))
  in
  set_color black;
  draw_side b.v1 b.v2 b.v3 b.v4 cp proj;
  set_color c1;
  draw_side b.v5 b.v6 b.v2 b.v1 cp proj;
  set_color c2;
  draw_side b.v1 b.v4 b.v8 b.v5 cp proj;
  set_color c2;
  draw_side b.v6 b.v7 b.v3 b.v2 cp proj;
  set_color c1;
  draw_side b.v7 b.v8 b.v4 b.v3 cp proj;
  set_color
    (rgb (int_of_float c.r) (int_of_float c.g) (int_of_float c.b));
  draw_side b.v8 b.v7 b.v6 b.v5 cp proj;
  ()

(** [box_closest_v3 p b] is the closest vertex of box [b] to point [p]
    in the xy plane *)
let box_closest_v3 (point : v3) (b : box) : v3 =
  box_fold_left
    (fun a b ->
      let pa = v3op ( -. ) point a in
      let pb = v3op ( -. ) point b in
      if
        (pa.x *. pa.x) +. (pa.z *. pa.z)
        < (pb.x *. pb.x) +. (pb.z *. pb.z)
      then a
      else b)
    (v3op_const ( *. ) Float.max_float (norm point))
    b

(** [box_farthest_v3 p b] is the farthest vertex of box [b] to point [p]
    in the xy plane *)
let box_farthest_v3 (point : v3) (b : box) : v3 =
  box_fold_left
    (fun a b ->
      let pa = v3op ( -. ) point a in
      let pb = v3op ( -. ) point b in
      if
        (pa.x *. pa.x) +. (pa.z *. pa.z)
        >= (pb.x *. pb.x) +. (pb.z *. pb.z)
      then a
      else b)
    { x = 0.; y = 0.; z = 0. }
    b

(** [separate boxes cp] takes camera position [cp] and boxes [boxes] and
    separates the boxes into (infront od z-axis line, behind z-axis
    line). This is used for rendering the z-axis infront of some boxes
    to further solidify the 3d projection rendering. *)
let rec separate (boxes : box list) (cp : v3) : box list * box list =
  let b1 =
    List.filter
      (fun b ->
        dot { cp with y = 0. } { (box_closest_v3 cp b) with y = 0. }
        > 0.)
      boxes
  in
  let b2 =
    List.filter
      (fun b ->
        dot { cp with y = 0. } { (box_closest_v3 cp b) with y = 0. }
        <= 0.)
      boxes
  in
  (b1, b2)

(** [sort_boxes cam b] sorts boxes [b] in terms of distance from camera
    position for layered drawing in 3d. This is done so that farther
    boxes can be drawn before, and therefore below, boxes that are
    closer to the camera. *)
let sort_boxes cam b =
  let cp = camera_pos cam in
  let b =
    List.sort
      (fun b1 b2 ->
        let min1 = v3op ( -. ) cp (box_closest_v3 cp b1) in
        let min2 = v3op ( -. ) cp (box_closest_v3 cp b2) in
        if mag { min1 with y = 0. } < mag { min2 with y = 0. } then 1
        else if mag min1 = mag min2 then 0
        else -1)
      b
  in
  separate b cp

(** [rotateX angle] is the x-axis rotation matrix with angle [angle] in
    degrees *)
let rotateX (angle : float) : m =
  let rad = angle *. pi /. 180. in
  [
    [ 1.; 0.; 0. ];
    [ 0.; cos rad; -1. *. sin rad ];
    [ 0.; sin rad; cos rad ];
  ]

(** [rotateY angle] is the y-axis rotation matrix with angle [angle] in
    degrees *)
let rotateY (angle : float) : m =
  let rad = angle *. pi /. 180. in
  [
    [ cos rad; 0.; sin rad ];
    [ 0.; 1.; 0. ];
    [ -1. *. sin rad; 0.; cos rad ];
  ]

(** [rotateZ angle] is the z-axis rotation matrix with angle [angle] in
    degrees *)
let rotateZ (angle : float) : m =
  let rad = angle *. pi /. 180. in
  [
    [ cos rad; -1. *. sin rad; 0. ];
    [ sin rad; cos rad; 0. ];
    [ 0.; 0.; 1. ];
  ]

(** [camera_proj cam p] projects point [p] onto a 2d plane using camera
    [cam] for rotation and weak-projection. *)
let camera_proj (cam : camera) (point : v3) : v3 =
  let proj = [ [ 1.; 0.; 0. ]; [ 0.; 1.; 0. ] ] in
  let rotated =
    m_to_v3
      (rotateZ cam.rz *** rotateX cam.rx *** rotateY cam.ry
     *** v3_to_m point)
  in
  let point = rotated |> v3op_const ( *. ) cam.s |> v3op ( +. ) cam.o in
  m_to_v3 (proj *** v3_to_m point)

(** [draw_text_v3 str proj p] draws text [str] at point [p] using
    projection function [proj] *)
let draw_text_v3 str proj (p : v3) : unit =
  let p = proj p in
  let _ = moveto (int_of_float p.x) (int_of_float p.y) in
  draw_string str

(** [draw_text_xy str x y] draws text [str] at position ([x], [y])*)
let draw_text_xy str (x : int) (y : int) : unit =
  let _ = moveto x y in
  draw_string str

(** [ticks proj len p dir] draws a tick mark on position [p] with length
    [len] and direction [dir] using projection function [proj] *)
let tick proj (len : float) (p : v3) (dir : v3) : unit =
  let dir = norm dir in
  let p1 = proj (v3op_const ( *. ) (len /. -2.) dir |> v3op ( +. ) p) in
  let p2 = proj (v3op_const ( *. ) (len /. 2.) dir |> v3op ( +. ) p) in
  draw_poly_line
    [|
      (int_of_float p1.x, int_of_float p1.y);
      (int_of_float p2.x, int_of_float p2.y);
    |];
  ()

(** [axis proj dir len] draws an axis with direction [dir] and length
    [len] that passes through origin using projection function [proj] *)
let axis proj (dir : v3) (len : float) : unit =
  let p1 = proj (v3op_const ( *. ) (len /. -2.) dir) in
  let p2 = proj (v3op_const ( *. ) (len /. 2.) dir) in
  draw_poly_line
    [|
      (int_of_float p1.x, int_of_float p1.y);
      (int_of_float p2.x, int_of_float p2.y);
    |];
  ()

(** [half_axis proj dir len] draws half an axis with direction [dir] and
    length [len] that starts at the origin using projection function
    [proj] *)
let half_axis proj (dir : v3) (len : float) : unit =
  let p1 = proj (v3op_const ( *. ) 0. dir) in
  let p2 = proj (v3op_const ( *. ) len dir) in
  draw_poly_line
    [|
      (int_of_float p1.x, int_of_float p1.y);
      (int_of_float p2.x, int_of_float p2.y);
    |];
  ()

(** [xz_axis cam proj len sx sz] draws the x and z axis with lengths
    [len] and ticks spaced [sx] and [sz] units apart. It is drawn using
    projection function [proj] and camera [cam].*)
let xz_axis cam proj len sx sz : unit =
  axis proj { x = 1.; y = 0.; z = 0. } len;
  draw_text_v3 "+x" proj { x = 300. /. cam.s; y = 0.; z = 25. /. cam.s };
  draw_text_v3 "-x" proj
    { x = -300. /. cam.s; y = 0.; z = 25. /. cam.s };
  axis proj { x = 0.; y = 0.; z = 1. } len;
  draw_text_v3 "-y" proj
    { x = 25. /. cam.s; y = 0.; z = -300. /. cam.s };
  draw_text_v3 "+y" proj { x = 25. /. cam.s; y = 0.; z = 300. /. cam.s };
  for
    i = int_of_float (-1. *. len /. 2. /. sx)
    to int_of_float (len /. 2. /. sx)
  do
    if i <> 0 then
      tick proj (10. /. cam.s)
        { x = float i *. sx; y = 0.; z = 0. }
        { x = 0.; y = 0.; z = 1. }
  done;
  for
    i = int_of_float (-1. *. len /. 2. /. sz)
    to int_of_float (len /. 2. /. sz)
  do
    if i <> 0 then
      tick proj (10. /. cam.s)
        { x = 0.; y = 0.; z = float i *. sz }
        { x = 1.; y = 0.; z = 0. }
  done;
  ()

(** [y_axis_top cam proj len sy] draws the top of the y axis with
    lengths [len] and ticks spaced [sy] units apart. It is drawn using
    projection function [proj] and camera [cam].*)
let y_axis_top cam proj len sy : unit =
  let _ = half_axis proj { x = 0.; y = 1.; z = 0. } (len /. 2.) in
  draw_text_v3 "prob" proj
    { x = 25. /. cam.s; y = 300. /. cam.s; z = 0. };
  let dir =
    cross { (camera_pos cam) with y = 0. } { x = 0.; y = 1.; z = 0. }
  in
  for i = 1 to int_of_float (len /. 2. /. sy) do
    tick proj (10. /. cam.s) { x = 0.; y = float i *. sy; z = 0. } dir;
    let label =
      norm dir
      |> v3op_const ( *. ) (20. /. cam.s)
      |> v3op ( +. ) { x = 0.; y = float i *. sy; z = 0. }
    in
    draw_text_v3 (string_of_float (float i)) proj label
  done

(** [y_axis_bottom cam proj len sy] draws the bottom of the y axis with
    lengths [len] and ticks spaced [sy] units apart. It is drawn using
    projection function [proj] and camera [cam].*)
let y_axis_bottom cam proj len sy : unit =
  let _ = half_axis proj { x = 0.; y = -1.; z = 0. } (len /. 2.) in
  let dir =
    cross { (camera_pos cam) with y = 0. } { x = 0.; y = 1.; z = 0. }
  in
  for i = int_of_float (-1. *. len /. 2. /. sy) to -1 do
    tick proj (10. /. cam.s) { x = 0.; y = float i *. sy; z = 0. } dir
  done

(** [updatecam cam proj key def] uses key input [key] to update camera
    reference [cam] and projection function [proj] to have the graph
    rotate and scale. *)
let updatecam cam proj key def =
  match key with
  | 'a' ->
      cam := { !cam with ry = !cam.ry -. 1. };
      proj := camera_proj !cam
  | 'd' ->
      cam := { !cam with ry = !cam.ry +. 1. };
      proj := camera_proj !cam
  | 'w' ->
      cam := { !cam with rx = min (max (!cam.rx +. 1.) 0.) 90. };
      proj := camera_proj !cam
  | 's' ->
      cam := { !cam with rx = min (max (!cam.rx -. 1.) 0.) 90. };
      proj := camera_proj !cam
  | 'e' ->
      cam := { !cam with s = min (!cam.s *. 1.1) 120. };
      proj := camera_proj !cam
  | 'q' ->
      cam := { !cam with s = min (!cam.s *. 0.9) 120. };
      proj := camera_proj !cam
  | 'r' ->
      cam := def;
      proj := camera_proj !cam
  | _ -> ()

(** [draw_axis_below cam proj key len sx sy sz] uses key input [key] to
    draw the axis with parameters [len], [sx], [sy], [sz] using camera
    [cam] and projection function [proj]. This is not done every tick,
    as ocaml graphics cannot support large number of render calls. So we
    only update the axis when the camera changes and therefore the graph
    changes position. *)
let draw_axis_below cam proj key len sx sy sz =
  let _ = set_color black in
  match key with
  | 'a' ->
      remember_mode true;
      clear_graph ();
      xz_axis cam proj len sx sz;
      y_axis_bottom cam proj len sy;
      remember_mode false
  | 'd' ->
      remember_mode true;
      clear_graph ();
      xz_axis cam proj len sx sz;
      y_axis_bottom cam proj len sy;
      remember_mode false
  | 'w' ->
      remember_mode true;
      clear_graph ();
      xz_axis cam proj len sx sz;
      y_axis_bottom cam proj len sy;
      remember_mode false
  | 's' ->
      remember_mode true;
      clear_graph ();
      xz_axis cam proj len sx sz;
      y_axis_bottom cam proj len sy;
      remember_mode false
  | 'e' ->
      remember_mode true;
      clear_graph ();
      xz_axis cam proj len sx sz;
      y_axis_bottom cam proj len sy;
      remember_mode false
  | 'q' ->
      remember_mode true;
      clear_graph ();
      xz_axis cam proj len sx sz;
      y_axis_bottom cam proj len sy;
      remember_mode false
  | 'r' ->
      remember_mode true;
      clear_graph ();
      xz_axis cam proj len sx sz;
      y_axis_bottom cam proj len sy;
      remember_mode false
  | _ -> ()

module type Graph2d = sig
  val graph_prob :
    Evolution.domain2d ->
    Complex.t list list ->
    Evolution.boundary_conditions ->
    unit
end

(** Make a 2d graphing module with Evolution2d solver [Solver] *)
module Make =
functor
  (Solver : Evolution2D)
  ->
  struct
    module S = Solver

    (** [graph_prob d2 t b] graphs the probability distribution for
        particles in a two dimensional domain given a set of two x and y
        domains [d2] a 2D list of initial conditions as complex numbers
        [t] and a boundary condition [b] for the solver to use *)
    let graph_prob
        (domain2d : Evolution.domain2d)
        (initial_condition : Complex.t list list)
        (boundary_condition : Evolution.boundary_conditions) =
      let _ = open_graph ":0 700x700" in
      let xdomain = fst domain2d in
      let xnum = List.length initial_condition in
      let width = (snd xdomain -. fst xdomain) /. float xnum in
      let zdomain = snd domain2d in
      let znum = List.length (List.hd initial_condition) in
      let length = (snd zdomain -. fst zdomain) /. float znum in
      let bbv1 = { x = fst xdomain; y = 0.; z = fst zdomain } in
      let bbv2 =
        {
          x = fst xdomain +. (width *. float xnum);
          y = 0.;
          z = fst zdomain +. (length *. float znum);
        }
      in
      let bounding_box = box bbv1 bbv2 in
      let s = min (600. /. (v3op ( -. ) bbv2 bbv1 |> mag)) 150. in
      let cam =
        ref
          {
            o = { x = 350.; y = 250.; z = 0. };
            s;
            rx = 25.;
            ry = -45.;
            rz = 0.;
          }
      in
      let def = !cam in
      let proj = ref (camera_proj !cam) in
      let sx = 1. in
      let sy = 0.7 *. (v3op ( -. ) bbv2 bbv1 |> mag) in
      let sz = 1. in
      let len = 500. in
      let _ = xz_axis !cam !proj len sx sz in
      let _ = y_axis_top !cam !proj len sy in
      let _ = y_axis_bottom !cam !proj len sy in
      let w = ref initial_condition in
      let t = ref 0. in
      let t_elapsed = ref 0. in
      try
        while true do
          let key = read_key () in
          synchronize ();
          remember_mode false;
          set_color (rgb 0 0 0);
          updatecam cam proj key def;
          let rep = S.from_list !w in
          let prob = S.probabilities rep in
          if key = ' ' then (
            t := Sys.time ();
            t_elapsed := !t_elapsed +. 0.1;
            w :=
              S.evolve rep 0.04 boundary_condition domain2d 0.2 false
              |> S.to_list);
          let max_box =
            ref
              (box
                 { x = 0.; y = 0.; z = 0. }
                 { x = 0.; y = 0.; z = 0. })
          in
          let boxes =
            List.mapi
              (fun xi prow ->
                List.mapi
                  (fun zi p ->
                    let x = (float xi *. width) +. fst xdomain in
                    let z = (float zi *. length) +. fst zdomain in
                    let b =
                      box { x; y = 0.; z }
                        { x = x +. width; y = p *. sy; z = z +. length }
                    in
                    let _ =
                      if p > !max_box.v5.y /. sy then max_box := b
                    in
                    b)
                  prow)
              prob
            |> List.flatten
          in
          let sort_box = sort_boxes !cam (!max_box :: boxes) in
          let _ = draw_axis_below !cam !proj key len sx sy sz in
          let _ =
            List.iteri
              (fun i b ->
                let bp =
                  box_closest_v3 (camera_pos !cam) bounding_box
                in
                let p = box_closest_v3 (camera_pos !cam) b in
                let len =
                  v3op ( -. ) { bp with y = 0. } { p with y = 0. }
                  |> mag
                in
                let max_len = v3op ( -. ) bbv2 bbv1 |> mag in
                let f = (len *. (1. -. 0.4) /. max_len) +. 0.4 in
                if box_bool ( = ) b !max_box then (
                  draw_box !cam !proj b
                    { r = 250. *. f; g = 150. *. f; b = 150. *. f };
                  set_color black;
                  let pos =
                    v3op ( +. )
                      (box_closest_v3 (camera_pos !cam) b)
                      (box_farthest_v3 (camera_pos !cam) b)
                    |> v3op_const ( /. ) 2.
                  in
                  let pos = { pos with y = b.v5.y } in
                  draw_text_v3 (string_of_float b.v5.y) !proj pos)
                else
                  draw_box !cam !proj b
                    { r = 250. *. f; g = 250. *. f; b = 250. *. f })
              (snd sort_box)
          in
          let _ = set_color black in
          let _ = y_axis_top !cam !proj len sy in
          List.iter
            (fun b ->
              let bp = box_closest_v3 (camera_pos !cam) bounding_box in
              let p = box_closest_v3 (camera_pos !cam) b in
              let len =
                v3op ( -. ) { bp with y = 0. } { p with y = 0. } |> mag
              in
              let max_len = v3op ( -. ) bbv2 bbv1 |> mag in
              let f = (len *. (1. -. 0.4) /. max_len) +. 0.4 in
              if box_bool ( = ) b !max_box then (
                draw_box !cam !proj b
                  { r = 250. *. f; g = 150. *. f; b = 150. *. f };
                set_color black;
                let pos =
                  v3op ( +. )
                    (box_closest_v3 (camera_pos !cam) b)
                    (box_farthest_v3 (camera_pos !cam) b)
                  |> v3op_const ( /. ) 2.
                in
                let pos = { pos with y = b.v5.y } in
                draw_text_v3 (string_of_float b.v5.y) !proj pos)
              else
                draw_box !cam !proj b
                  { r = 250. *. f; g = 250. *. f; b = 250. *. f })
            (fst sort_box);
          set_color black;

          draw_text_xy
            ("Time: " ^ string_of_float !t_elapsed)
            5
            (size_y () - 15)
        done
      with Exit -> ()
  end
