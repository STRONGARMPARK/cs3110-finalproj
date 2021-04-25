open Evolution;;

module type Graph = sig
  val graph_prob : Evolution.domain -> Complex.t list -> Evolution.boundary_conditions -> unit
  val graph_wave : Evolution.domain -> Complex.t list -> Evolution.boundary_conditions -> unit
end

module Make : functor (Solver : Evolution1D) -> Graph
