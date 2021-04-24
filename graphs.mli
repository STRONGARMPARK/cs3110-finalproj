module type Graph = sig
  val graph : 'a -> unit
end

module Grapher : Graph