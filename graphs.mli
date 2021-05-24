open Evolution

(** [vop op a b] takes 2d vertex [a] and 2d vertex [b] and applies
    operation [op] to them *)
val vop : ('a -> 'b -> 'c) -> 'a * 'a -> 'b * 'b -> 'c * 'c

(** [vfloat a] is the 2d float vertex from 2d int vertex [a] *)
val vfloat : int * int -> float * float

(** [vint a] is the 2d int vertex from 2d float vertex [a] *)
val vint : float * float -> int * int

(** [mag_float a] is the magnitude of 2d float vertex [a] *)
val mag_float : float * float -> float

(** [mag_int a] is the magnitude of 2d int vertex [a] *)
val mag_int : int * int -> float

(** A [Graph] contains a method for graphing probabilities in a 1d
    domain *)
module type Graph = sig
  (** [graph_prob d t b] graphs the probability distribution for
      particles in a one dimensional domain given a domain [d] a list of
      initial conditions as complex numbers [t] in a boundary condition
      [b] for the solver to use *)
  val graph_prob :
    Evolution.domain ->
    Complex.t list ->
    Evolution.boundary_conditions ->
    unit
end

module Make : functor (Solver : Evolution1D) -> Graph
