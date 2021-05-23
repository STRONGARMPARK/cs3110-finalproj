open Evolution

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
