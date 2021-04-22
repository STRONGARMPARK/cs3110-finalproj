type boundary_conditions = 
  |Periodic
  |Dirichlet
  |Neumann of (Complex.t*Complex.t)

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
    |(a, b) -> 
      if b > a then b -. a
      else raise (Invalid_argument "Invalid domain (b <= a)")
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

module FreeParticleEvolutionSpectral1D : Evolution1D = struct
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

let second_derivative (w : Complex.t array) (b : boundary_conditions) 
  (d : domain) (n : int) : Complex.t array = 
  let l = 
    match d with
    |(a, b) -> b -. a
  in
  let dx = l /. (Float.of_int n) in
  let wxx = Array.make n Complex.zero in
  
  let boundary b wxx = 
    match b with
    |Periodic -> 
      wxx.(0) <- 
        Complex.div (Complex.add w.(1) (Complex.neg w.(n-1))) 
          {re=2. *. dx;im=0.};
      wxx.(n-1) <- 
        Complex.div (Complex.add w.(0) (Complex.neg w.(n-2))) 
          {re=2. *. dx;im=0.}
    |Dirichlet ->
      wxx.(0) <- Complex.zero;
      wxx.(n-1) <- Complex.zero
    |Neumann (a, b) ->
      wxx.(0) <- a;
      wxx.(n-1) <- b
  in
  boundary b wxx;
  
  for i=1 to n-2 do 
    wxx.(i) <-
      Complex.div (Complex.add w.(i+1) (Complex.neg w.(i-1))) 
        {re=2. *. dx;im=0.};
  done;
    
  wxx

module FreeParticleEvolutionEulers1D = struct
  (** AF: t represents the wave function as an array of complex numbers,
    the wave function evaluated at each point in the discretized domain. 
    In other words, if w : t, then w.(i) is the evaluation of w at the
    (i+1)th point in the domain.*)
  
  type t = Complex.t array

  (** Allowed boundary conditions. Must feed dummy values to constructor 
    Neumann to properly type check. You may ignore those values. *)
  let boundary_condition = [Periodic; Dirichlet; 
    Neumann (Complex.zero, Complex.zero)]

  let normalize = FreeParticleEvolutionSpectral1D.normalize

  let from_list vec = 
    normalize vec |> Array.of_list
  
  let to_list = 
    Array.to_list
  
  let probabilities w = 
    w |> to_list |> List.map Complex.norm2

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

let get_x (n : int) (d : domain) = 
  let x = Array.make n 0. in
  match d with
  |(a, b) -> 
    if (0.-.b <> a) then
      raise (Invalid_argument "Domain must be symmetric about the origin.")
    else
      let dx = (b-.a) /. (Float.of_int (n-1)) in
      for i = 0 to n - 1 do
        x.(i) <- a +. ((Float.of_int i) *. dx);
      done;
      x

let get_x2 (n : int) (d : domain) = 
  let x = get_x n d in
  Array.map (fun y -> y*.y) x

module HarmonicOscillatorEvolutionEulers1D = struct

  type t = Complex.t array

  (** Allowed boundary conditions. Must feed dummy values to constructor 
    Neumann to properly type check. You may ignore those values. *)
  let boundary_condition = [Periodic; Dirichlet; 
    Neumann (Complex.zero, Complex.zero)]

  let normalize = FreeParticleEvolutionSpectral1D.normalize

  let from_list vec = 
    normalize vec |> Array.of_list
  
  let to_list = 
    Array.to_list
  
  let probabilities w = 
    w |> to_list |> List.map Complex.norm2

  let step w tau b d =
    let n = Array.length w in
    let wxx = second_derivative w b d n in
    let x2 = get_x2 n d in

    Array.map (fun x -> Complex.mul ({re = 0.;im = tau /. 2.}) x) wxx
    |> (Array.map2 Complex.add) 
      (Array.map (fun y -> {Complex.re=0.;im = -1. *. y *. tau /. 2.}) x2)
    |> (Array.map2 Complex.add) w
  
  let step_mutate w tau b d n = 
    let wxx = second_derivative w b d n in
    let x2 = get_x2 n d in
    for i = 0 to n - 1 do 
      w.(i) <- 
        (Complex.add (Complex.mul {re = 0.;im = tau /. 2.} wxx.(i)) w.(i)
        |> (Complex.add {Complex.re=0.;im = -1. *. x2.(i) *. tau /. 2.}))
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

type domain2d = domain * domain

module type Evolution2D = sig

  (** Representation type of function *)
  type t

  (** List to represent the different allowed 
    kinds of boundary conditions. *)
  val boundary_condition : boundary_conditions list

  (** Function to normalize a given vector representation
    of a wave function *)
  val normalize : (Complex.t list) list -> (Complex.t list) list

  (** Function to convert from vector representation 
    of wave function to representation type*)
  val from_list :  (Complex.t list) list -> t

  (** Function to convert from representation type
    to vector representation of wave function*)
  val to_list : t -> (Complex.t list) list

  (** Function to convert from representation type
    to vector representation of probability distribution
    (Psi^dag * Psi) *)
  val probabilities : t -> (float list) list

  (** Function to evolve a wavefunction in time for a single timestep. 
  Returns the internal representation of the wavefunction after one timestep*)
  val step : t -> float -> boundary_conditions -> domain2d -> t

  (** Function to evolve a wavefunction in time
    for a given time (in seconds). Returns the internal
    representation of the wavefunction after the last timestep.
    Boolean argument tells function whether to print
    each timestep in a file. *)
  val evolve : t -> float -> boundary_conditions -> domain2d -> float -> bool -> t

end

(** [transpose mat] returns the matrix transpose of the 2d list mat *)
let rec transpose mat = 
  match mat with
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss ->
    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

(** [col_map f mat] returns mat with each column of mat replaced with
  the function f applied to that column. f therefore must take in an 'a list
  and returns a 'b list of the same length. *)
let rec col_map f mat = 
  match mat with
  |[] -> []
  |h :: t -> f h :: col_map f t

(** [row_map f mat] returns map with each row of mat replaced with
  the function f applied to that column. f therefore must take in an 'a list
  and returns a 'b list of the same length. *)
let row_map f mat = 
  mat |> transpose |> col_map f |> transpose

(** Fast fourier transform of the 2d list mat, with each dimension having 
    n elements.
    Precondition: n is a power of 2 *)
let re_fft n vec = fft vec n
let re_ifft n vec = ifft vec n

let fft2 mat n m = mat |> row_map (re_fft n) |> col_map (re_fft m)

(** Inverse fast fourier transform of the 2d list mat, with each dimension 
    having n elements.
    Precondition: n is a power of 2 *)
let ifft2 mat n m = 
  mat |> List.rev |> col_map (re_ifft m) |> List.rev
  |> transpose |> List.rev |> transpose |> row_map (re_ifft n) 
  |> transpose |> List.rev |> transpose


let get_k2_2d (n : int) (m : int) (d2 : domain2d) =
  List.map2 (fun x y -> x +. y ) (get_k2 n (fst d2)) (get_k2 m (snd d2))

module FreeParticleEvolutionSpectral2D : Evolution2D = struct
  (** AF: t represents the 2D (discrete) fourier transform of a 
    wave function. *)
  type t = (Complex.t list) list
  
  let boundary_condition = [Periodic]

  let norm1d vec = 
    List.fold_left (fun acc x -> acc +. Complex.norm2 x) 0. vec |> sqrt

  let div1d vec norm = 
    List.map (fun x -> Complex.div x {re = norm; im = 0.}) vec
  
  let normalize mat = 
    let rec norm mat =
      match mat with
      |[] -> 0.
      |h :: t -> (norm1d h) +. (norm t)
    in
    let matnorm = norm mat in
    let rec div mat = 
      match mat with
      |[] -> []
      |h :: t -> (div1d h matnorm) :: (div t)
    in
    div mat
  
  (** [from_list mat] returns internal representation of mat, of type t.
      Precondition: mat is a Complex.t list list of length n by m, where
      n and m are both powers of 2.*)
  let from_list mat = 
    (mat |> normalize |> fft2) (List.length mat) (List.length (List.hd mat))

  let to_list w =
    (w |> ifft2) (List.length w) (List.length (List.hd w))

  let probabilites1d vec =
    vec |> List.map Complex.norm2
  
  let probabilities w = 
    let mat = to_list w in
    let rec probs mat =
      match mat with
      |[] -> []
      |h :: t -> (probabilites1d h) :: (probs t)
    in
    probs mat

  let step w tau b d2 = 
    if b <> Periodic then 
      raise (Invalid_argument "Illegal boundary condition.");
    let n = List.length w in
    let m = List.length (List.hd w) in
    let k2 = get_k2_2d n m d2 in
    let f w1d = 
      List.map (fun x -> Complex.exp {Complex.re = 0.; im = -0.5 *. tau *. x}) k2
      |> List.map2 Complex.mul w1d
    in
    col_map f w

  let evolve w tau b d2 time print =
    step w time b d2
    (*TODO: Implement printing *)
end

(**
module FreeParticleEvolutionEulers2D : Evolution2D = struct

end
*)