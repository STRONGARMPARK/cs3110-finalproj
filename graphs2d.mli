open Evolution;;

module type Graph = sig
  val graph_prob : Evolution.domain2d -> (Complex.t list) list -> Evolution.boundary_conditions list -> unit
  val graph_wave : Evolution.domain2d -> (Complex.t list) list -> Evolution.boundary_conditions list -> unit
end

module Make : functor (Solver : Evolution2D) -> Graph 