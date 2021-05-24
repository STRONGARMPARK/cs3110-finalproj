open Evolution

exception NotAMatrix of string

type m = float list list

type v = float list

type v3 = {
  x : float;
  y : float;
  z : float;
}

type c = {
  r : float;
  g : float;
  b : float;
}

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

(** [int_to_c i] take an integer [i] and returns a color type [c] with
    red, green and blue values *)
val int_to_c : int -> c

(** [v3pop_const op b a] takes an float operation [op] and applies it to
    3D vertex [a] with constant [b]. Applies operator to each vertex
    value seperately. Order of operations is a [op] b. ex. a +. b *)
val v3op_const : (float -> float -> float) -> float -> v3 -> v3

(** [v3pop op b a] takes an float operation [op] and applies it to 3D
    vertex [a] and 3d vertex [b]. Applies operator to each vertex value
    seperately. Order of operations is a [op] b. ex. a \+. b *)
val v3op : (float -> float -> float) -> v3 -> v3 -> v3

(** [v3pop_bool op b a] takes an boolean operator [op] and applies it to
    3D vertex [a] and 3d vertex [b]. Applies operator to each vertex
    value seperately. Order of operations is a [op] b. ex. a = b *)
val v3op_bool : (float -> float -> bool) -> v3 -> v3 -> bool

(** [v3pop_bool_const op b a] takes an boolean operator [op] and applies
    it to 3D vertex [a] and float [b]. Applies operator to each vertex
    value seperately. Order of operations is a [op] b. ex. a = b *)
val v3op_bool_const : (float -> float -> bool) -> float -> v3 -> bool

(** [box_bool op a b] takes an boolean operator [op] and applies it to
    box [a] and box [b]. Applies operator to each vertex value
    seperately. Order of operations is a [op] b. ex. a = b *)
val box_bool : (v3 -> v3 -> bool) -> box -> box -> bool

(** [to_float i] takes a 2D list of integers and converts it into a 2D
    list of floats with type [m] *)
val to_float : int list list -> m

(** [v3_to_m v3] takes a 3D vertex [v3] and transforms it into a matrix
    [m] *)
val v3_to_m : v3 -> m

(** [m_to_v m] takes a matrix [m] and transforms it into a vertex [v].
    If matrix [m] has more rows than 1, it only uses the first row *)
val m_to_v : m -> v

(** [v3_to_v v3] takes a 3D vertex [v3] and transforms it into a vertex
    [v]. *)
val v3_to_v : v3 -> v

(** [v_to_v3 v] takes an N dimension vertex [v] and transforms it into a
    3d vertex [v3] by getting the first 3 values, substituting 0 for all
    unknown values. *)
val v_to_v3 : v -> v3

(** [check m] checks if m is a valid matrix. A valid matrix is
    rectangle, so has the same number of columns for each row, or the
    same number of rows for each column. *)
val check : m -> m

(** [transpose m] transposes matrix [m]. A transposed matrix of size ...
    (n x m) has size (m x n)*)
val transpose : m -> m

(** [m_to_v3 m] turns matrix [m] into a 3d vector of type [v3] *)
val m_to_v3 : m -> v3

(** [dot a b] applies a dot product operation to 3D vertices a and b.
    Order of operations is a dot b *)
val dot : v3 -> v3 -> float

(** [cross a b] applies a cross product operation to 3D vertices a and
    b. Order of operations is a cross b *)
val cross : v3 -> v3 -> v3

(** [(**) a b] is the dot product of two vertices of N dimensions with
    type [v] *)
val ( ** ) : v -> v -> float

(** [(***) a b] multiplies matrix [a] by matrix [b] *)
val ( *** ) : m -> m -> m

(** [mag p] is the magnitude of 3D vector [p] *)
val mag : v3 -> float

(** [norm p] is the normalized 3D vector of 3D vector [p] *)
val norm : v3 -> v3

(** [box a b] is the [box] type with 3D vectors [a] and [b] as two
    opposite corners of the box.*)
val box : v3 -> v3 -> box

(** [box_map f b] applies function [f] to each vertex in box [b]
    seperately *)
val box_map : (v3 -> v3) -> box -> box

(** [box_fold_left f init b] is f (... (f (f init v1) v2) ...) v8] where
    v1...v8 are vertices in box [b] *)
val box_fold_left : ('a -> v3 -> 'a) -> 'a -> box -> 'a

(** [box_closest_v3 p b] is the closest vertex of box [b] to point [p]
    in the xy plane *)
val box_closest_v3 : v3 -> box -> v3

(** [box_farthest_v3 p b] is the farthest vertex of box [b] to point [p]
    in the xy plane *)
val box_farthest_v3 : v3 -> box -> v3

(** [rotateX angle] is the x-axis rotation matrix with angle [angle] in
    degrees *)
val rotateX : float -> m

(** [rotateY angle] is the y-axis rotation matrix with angle [angle] in
    degrees *)
val rotateY : float -> m

(** [rotateZ angle] is the z-axis rotation matrix with angle [angle] in
    degrees *)
val rotateZ : float -> m

(** A [Graph2d] contains a method for graphing probabilities in a 2d
    domain *)
module type Graph2d = sig
  (** [graph_prob d2 t b] graphs the probability distribution for
      particles in a two dimensional domain given a set of two x and y
      domains [d2] a 2D list of initial conditions as complex numbers
      [t] in a boundary condition [b] for the solver to use *)
  val graph_prob :
    Evolution.domain2d ->
    Complex.t list list ->
    Evolution.boundary_conditions ->
    unit
end

module Make : functor (Solver : Evolution2D) -> Graph2d
