open Evolution1d;;

module type Graph = sig
  val graph : 'a -> unit
end

module Make : functor (Solver : Evolution1D) -> Graph
