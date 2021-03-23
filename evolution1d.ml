module type Evolution1D = sig

  (** Representation type of function *)
  type t

  type boundary_condition

  (** Domain of the wavefunction. *)
  val domain : float * float

  (** Time step for numerical method *)
  val tau : int

  (** Function to normalize a given vector representation
    of a wave function *)
  val normalize : Complex.t list -> Complex.t list

  (** Function to convert from vector representation 
    of wave function to representation type*)
  val from_list :  Complex.t list -> t

  (** Function to convert from representation type
    to vector representation of wave function*)
  val to_list : t -> Complex.t list

  (** Function to convert from representation type
    to vector representation of probability distribution
    (Psi^dag * Psi) *)
  val probabilites : t -> float list

  (** Function to evolve a wavefunction in time for a single timestep. 
  Returns the internal representation of the wavefunction after one timestep*)
  val step : t -> t

  (** Function to evolve a wavefunction in time
    for a given time (in seconds). Returns the internal
    representation of the wavefunction after the last timestep.
    Boolean argument tells function whether to print
    each timestep in a file. *)
  val evolve : t -> int -> bool -> t

end

