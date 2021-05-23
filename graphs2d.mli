open Evolution

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
