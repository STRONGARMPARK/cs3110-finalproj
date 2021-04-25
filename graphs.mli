open Evolution1d;;

module type Graph = sig
  val graph_prob : Evolution1d.domain -> Complex.t list -> Evolution1d.boundary_conditions -> unit
  val graph_wave : Evolution1d.domain -> Complex.t list -> Evolution1d.boundary_conditions -> unit
end

module Make : functor (Solver : Evolution1D) -> Graph
