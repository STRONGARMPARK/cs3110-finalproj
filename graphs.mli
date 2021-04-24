open Evolution1d;;

module type Graph = sig
  val graph : 'a -> unit
end

module Grapher : Graph

module type Make = functor (Solver : Evolution1D) -> Graph

module Make = functor (Solver : Evolution1D) -> Graph