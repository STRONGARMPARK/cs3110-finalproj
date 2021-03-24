type boundary_conditions = 
  |Periodic
  |Dirichlet
  |Neumann

type domain = (float * float)

module type Evolution1D = sig

  (** Representation type of function *)
  type t

  (** List to represent the different allowed 
    kinds of boundary conditions. *)
  val boundary_condition : boundary_conditions list

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
  val probabilities : t -> float list

  (** Function to evolve a wavefunction in time for a single timestep. 
  Returns the internal representation of the wavefunction after one timestep*)
  val step : t -> float -> boundary_conditions -> domain -> t

  (** Function to evolve a wavefunction in time
    for a given time (in seconds). Returns the internal
    representation of the wavefunction after the last timestep.
    Boolean argument tells function whether to print
    each timestep in a file. *)
  val evolve : t -> float -> boundary_conditions -> domain -> float -> bool -> t

end

module FreeParticleEvolutionSpectral : Evolution1D = struct
  (** AF: t represents the (discrete) fourier transform of a 
    wave function. *)
  type t = Complex.t list

  let boundary_condition = [Periodic]
  
  (** [normalize vec] returns vec normalized to a norm of 1.*)
  let normalize vec =
    let norm = 
      List.fold_left (fun acc x -> acc +. Complex.norm2 x) 0. vec |> sqrt
    in
    List.map (fun x -> Complex.div x {re = norm; im = 0.}) vec

  let pi = Float.pi

  (** Helper function for fft. Splits vec into two lists, containing
    it's elements with even and odd indices, respectively. O(n) efficiency.*)
  let split vec = 
    let rec splitter vec evens odds count = 
      match vec with
      |[] -> (List.rev evens, List.rev odds)
      |h :: t -> begin
        if count mod 2 = 0 then splitter t (h :: evens) odds (count + 1)
        else splitter t evens (h :: odds) (count + 1)
      end
    in
    splitter vec [] [] 0
  
  let root_of_unity n j =
    Complex.exp {re = 0.; im = -2. *. pi *. Float.of_int(j) /. Float.of_int(n)}

  let fft_build fft_evens fft_odds n = 
    let arr_evens = Array.of_list fft_evens in
    let arr_odds = Array.of_list fft_odds in
    let arr = Array.make n (Complex.mul Complex.one {re = 0.; im = 0.}) in
    for i = 0 to n/2 - 1 do 
      arr.(i) <- Complex.add arr_evens.(i) 
        (Complex.mul arr_odds.(i) (root_of_unity n i));
      arr.(i+n/2) <- Complex.add arr_evens.(i) 
        (Complex.mul arr_odds.(i) (root_of_unity n (i+n/2)));
    done;

    Array.to_list arr

  (** Fast fourier transform of vec with n elements. O(nlog(n)) efficiency 
    Precondition: n is a power of 2.*)
  let rec fft (vec : Complex.t list) (n : int) : Complex.t list = 
    match n with
    |1 -> vec
    |_ -> 
      let (evens, odds) = split vec in
      let fft_evens = fft evens (n/2) in
      let fft_odds = fft odds (n/2) in
      fft_build fft_evens fft_odds n

  let cswap (c : Complex.t) : Complex.t = 
    match c with
    |{re=a; im=b} -> {re=b ; im=a}

  (** Inverse fast fourier transform of vec with n elements. O(nlog(n)) efficiency 
    Precondition: n is a power of 2.*)
  let ifft (vec : Complex.t list) (n : int) : Complex.t list = 
    (List.map cswap vec |> fft) n |> List.map cswap
      |> List.map (fun x -> Complex.div x {re = (Float.of_int n); im = 0.})

  let get_k (n : int) (d : domain) = 
    let l = 
      match d with
      |(a, b) -> b -. a
    in 
    let k = Array.of_list 
      (List.map Float.of_int (List.init n (fun x -> x-n/2))) 
    in
    let k_new = Array.make n {Complex.re=0.;im=0.} in
    for i=0 to n-1 do
      k_new.(i) <- {Complex.re = 2. *. pi *. k.(i) /. l; im=0.};
    done;
    Array.to_list k_new

  let get_k2 (n : int) (d : domain) =
    List.map Complex.norm2 (get_k n d)

  (** [from_list] vec n returns the discrete fourier transform of vec
    with n elements. Precondition: n must be a power of 2.*)
  let from_list vec = 
    fft (normalize vec) (List.length vec)

  let to_list w = 
    ifft w (List.length w)

  let probabilities w = 
    to_list w |> List.map Complex.norm2

  let step w tau b d = 
    if b <> Periodic then 
      raise (Invalid_argument "Illegal boundary condition.");
    let n = List.length w in
    let k2 = get_k2 n d in
    List.map (fun x -> Complex.exp {Complex.re = 0.; im = -0.5 *. tau *. x}) k2
    |> List.map2 Complex.mul w

  let evolve w tau b d time print =
    step w time b d
    (*TODO: Implement printing *)
end

module FreeParticleEvolutionEulers = struct
  (** AF: t represents the wave function as an array of complex numbers,
    the wave function evaluated at each point in the discretized domain. 
    In other words, if w : t, then w.(i) is the evaluation of w at the
    (i+1)th point in the domain.*)
  
  type t = Complex.t array

  let boundary_condition = [Periodic; Dirichlet]

  let normalize = FreeParticleEvolutionSpectral.normalize

  let from_list vec = 
    normalize vec |> Array.of_list
  
  let to_list = 
    Array.to_list
  
  let probabilities w = 
    w |> to_list |> List.map Complex.norm2

  let second_derivative w b d n = 
    if b <> Periodic && b <> Dirichlet then 
      raise (Invalid_argument "Illegal boundary condition.");
    let l = 
      match d with
      |(a, b) -> b -. a
    in
    let dx = l /. (Float.of_int n) in
    let wxx = Array.make n Complex.zero in
    
    
    if b = Periodic then
      wxx.(0) <- 
        Complex.div (Complex.add w.(1) (Complex.neg w.(n-1))) 
          {re=2. *. dx;im=0.};
      wxx.(n-1) <- 
        Complex.div (Complex.add w.(0) (Complex.neg w.(n-2))) 
          {re=2. *. dx;im=0.};
    for i=1 to n-2 do 
      wxx.(i) <-
        Complex.div (Complex.add w.(i+1) (Complex.neg w.(i-1))) 
          {re=2. *. dx;im=0.};
    done;
    
    wxx

  let step w tau b d =
    let n = Array.length w in
    let wxx = second_derivative w b d n in
    Array.map (fun x -> Complex.mul ({re = 0.;im = tau /. 2.}) x) wxx
    |> (Array.map2 Complex.add) w

  
  let step_mutate w tau b d n = 
    let wxx = second_derivative w b d n in
    for i = 0 to n - 1 do 
      w.(i) <- 
        Complex.add (Complex.mul {re = 0.;im = tau /. 2.} wxx.(i)) w.(i);
    done

  let evolve w tau b d time print = 
    let n = Array.length w in
    let wnew = Array.copy w in
    let steps = Float.to_int (Float.ceil (time /. tau)) in
    for i = 0 to steps do
      (*TODO: Implement printing*)
      step_mutate wnew tau b d n;
    done;
    
    wnew
end