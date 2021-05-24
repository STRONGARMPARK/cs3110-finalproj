type boundary_conditions =
  | Periodic
  | Dirichlet
  | Neumann of (Complex.t * Complex.t)

type domain = float * float

module type Evolution1D = sig
  (** Representation type of function *)
  type t

  (** Represents maximum timestep value *)
  val dt : float

  (** List to represent the different allowed kinds of boundary
      conditions. *)
  val boundary_condition : boundary_conditions list

  (** Function to normalize a given vector representation of a wave
      function *)
  val normalize : Complex.t list -> Complex.t list

  (** Function to convert from vector representation of wave function to
      representation type*)
  val from_list : Complex.t list -> t

  (** Function to convert from representation type to vector
      representation of wave function*)
  val to_list : t -> Complex.t list

  (** Function to convert from representation type to vector
      representation of probability distribution (Psi^dag * Psi) *)
  val probabilities : t -> float list

  (** Function to evolve a wavefunction in time for a single timestep.
      Returns the internal representation of the wavefunction after one
      timestep*)
  val step : t -> float -> boundary_conditions -> domain -> t

  (** Function to evolve a wavefunction in time for a given time (in
      seconds). Returns the internal representation of the wavefunction
      after the last timestep. Boolean argument tells function whether
      to print each timestep in a file. *)
  val evolve :
    t -> float -> boundary_conditions -> domain -> float -> bool -> t
end

let pi = Float.pi

(** Helper function for fft. Splits vec into two lists, containing it's
    elements with even and odd indices, respectively. O(n) efficiency.*)
let split vec =
  let rec splitter vec evens odds count =
    match vec with
    | [] -> (List.rev evens, List.rev odds)
    | h :: t ->
        if count mod 2 = 0 then splitter t (h :: evens) odds (count + 1)
        else splitter t evens (h :: odds) (count + 1)
  in
  splitter vec [] [] 0

let root_of_unity n j =
  Complex.exp
    { re = 0.; im = -2. *. pi *. Float.of_int j /. Float.of_int n }

let fft_build fft_evens fft_odds n =
  let arr_evens = Array.of_list fft_evens in
  let arr_odds = Array.of_list fft_odds in
  let arr =
    Array.make n (Complex.mul Complex.one { re = 0.; im = 0. })
  in
  for i = 0 to (n / 2) - 1 do
    arr.(i) <-
      Complex.add arr_evens.(i)
        (Complex.mul arr_odds.(i) (root_of_unity n i));
    arr.(i + (n / 2)) <-
      Complex.add arr_evens.(i)
        (Complex.mul arr_odds.(i) (root_of_unity n (i + (n / 2))))
  done;

  Array.to_list arr

(** Fast fourier transform of vec with n elements. O(nlog(n)) efficiency
    Precondition: n is a power of 2.*)
let rec fft (vec : Complex.t list) (n : int) : Complex.t list =
  match n with
  | 1 -> vec
  | _ ->
      let evens, odds = split vec in
      let fft_evens = fft evens (n / 2) in
      let fft_odds = fft odds (n / 2) in
      fft_build fft_evens fft_odds n

let cswap (c : Complex.t) : Complex.t =
  match c with { re = a; im = b } -> { re = b; im = a }

(** Inverse fast fourier transform of vec with n elements. O(nlog(n))
    efficiency Precondition: n is a power of 2.*)
let ifft (vec : Complex.t list) (n : int) : Complex.t list =
  (List.map cswap vec |> fft) n
  |> List.map cswap
  |> List.map (fun x -> Complex.div x { re = Float.of_int n; im = 0. })

let get_k (n : int) (d : domain) =
  let l =
    match d with
    | a, b ->
        if b > a then b -. a
        else raise (Invalid_argument "Invalid domain (b <= a)")
  in
  let k =
    Array.of_list
      (List.map Float.of_int (List.init n (fun x -> x - (n / 2))))
  in
  let k_new = Array.make n { Complex.re = 0.; im = 0. } in
  for i = 0 to n - 1 do
    k_new.(i) <- { Complex.re = 2. *. pi *. k.(i) /. l; im = 0. }
  done;
  Array.to_list k_new

let get_k2 (n : int) (d : domain) = List.map Complex.norm2 (get_k n d)

module FreeParticleEvolutionSpectral1D : Evolution1D = struct
  (** AF: t represents the (discrete) fourier transform of a wave
      function. *)
  type t = Complex.t list

  let dt = 0.01

  let boundary_condition = [ Periodic ]

  (** [normalize vec] returns vec normalized to a norm of 1.*)
  let normalize vec =
    let norm =
      List.fold_left (fun acc x -> acc +. Complex.norm2 x) 0. vec
      |> sqrt
    in
    List.map (fun x -> Complex.div x { re = norm; im = 0. }) vec

  (** [from_list] vec n returns the discrete fourier transform of vec
      with n elements. Precondition: n must be a power of 2.*)
  let from_list vec = fft (normalize vec) (List.length vec)

  let to_list w = ifft w (List.length w)

  let probabilities w = to_list w |> List.map Complex.norm2

  let step w tau b d =
    if b <> Periodic then
      raise (Invalid_argument "Illegal boundary condition.");
    let n = List.length w in
    let k2 = get_k2 n d in
    List.map (fun x -> Complex.exp {Complex.re = 0.; im = -0.5 *. tau *. x}) k2
    |> (List.map2 Complex.mul w)

  let evolve w tau b d time print = step w time b d

  (*TODO: Implement printing *)
end

let second_derivative
    (w : Complex.t array)
    (b : boundary_conditions)
    (d : domain)
    (n : int) : Complex.t array =
  let l = match d with a, b -> b -. a in
  let dx = l /. Float.of_int n in
  let wxx = Array.make n Complex.zero in

  let boundary b wxx =
    match b with
    | Periodic ->
        wxx.(0) <-
          Complex.div
            (Complex.add w.(1) (Complex.neg w.(n - 1)))
            { re = 2. *. dx; im = 0. };
        wxx.(n - 1) <-
          Complex.div
            (Complex.add w.(0) (Complex.neg w.(n - 2)))
            { re = 2. *. dx; im = 0. }
    | Dirichlet ->
        wxx.(0) <- Complex.zero;
        wxx.(n - 1) <- Complex.zero
    | Neumann (a, b) ->
        wxx.(0) <- a;
        wxx.(n - 1) <- b
  in
  boundary b wxx;

  for i = 1 to n - 2 do
    wxx.(i) <-
      Complex.div
        (Complex.add w.(i + 1) (Complex.neg w.(i - 1)))
        { re = 2. *. dx; im = 0. }
  done;

  wxx

module FreeParticleEvolutionEulers1D = struct
  (** AF: t represents the wave function as an array of complex numbers,
      the wave function evaluated at each point in the discretized
      domain. In other words, if w : t, then w.(i) is the evaluation of
      w at the (i+1)th point in the domain.*)

  type t = Complex.t array

  let dt = 0.0001

  (** Allowed boundary conditions. Must feed dummy values to constructor
      Neumann to properly type check. You may ignore those values. *)
  let boundary_condition =
    [ Periodic; Dirichlet; Neumann (Complex.zero, Complex.zero) ]

  let normalize = FreeParticleEvolutionSpectral1D.normalize

  let from_list vec = normalize vec |> Array.of_list

  let to_list w = Array.to_list w |> normalize

  let probabilities w = w |> to_list |> List.map Complex.norm2

  let step w tau b d =
    let n = Array.length w in
    let wxx = second_derivative w b d n in
    Array.map (fun x -> Complex.mul { re = 0.; im = tau /. 2. } x) wxx
    |> (Array.map2 Complex.add) w

  let step_mutate w tau b d n =
    let wxx = second_derivative w b d n in
    for i = 0 to n - 1 do
      w.(i) <-
        Complex.add
          (Complex.mul { re = 0.; im = tau /. 2. } wxx.(i))
          w.(i)
    done

  let evolve w tau b d time print =
    let n = Array.length w in
    let wnew = Array.copy w in
    let steps = Float.to_int (Float.ceil (time /. tau)) in
    for i = 0 to steps do
      (*TODO: Implement printing*)
      step_mutate wnew tau b d n
    done;

    wnew
end

let get_x (n : int) (d : domain) =
  let x = Array.make n 0. in
  match d with
  | a, b ->
      if 0. -. b <> a then
        raise
          (Invalid_argument "Domain must be symmetric about the origin.")
      else
        let dx = (b -. a) /. Float.of_int (n - 1) in
        for i = 0 to n - 1 do
          x.(i) <- a +. (Float.of_int i *. dx)
        done;
        x

let get_x2 (n : int) (d : domain) =
  let x = get_x n d in
  Array.map (fun y -> y *. y) x

module HarmonicOscillatorEvolutionEulers1D = struct
  type t = Complex.t array

  let dt = 0.0001

  (** Allowed boundary conditions. Must feed dummy values to constructor
      Neumann to properly type check. You may ignore those values. *)
  let boundary_condition =
    [ Periodic; Dirichlet; Neumann (Complex.zero, Complex.zero) ]

  let normalize = FreeParticleEvolutionSpectral1D.normalize

  let from_list vec = normalize vec |> Array.of_list

  let to_list w = Array.to_list w |> normalize

  let probabilities w = w |> to_list |> List.map Complex.norm2

  let step w tau b d =
    let n = Array.length w in
    let wxx = second_derivative w b d n in
    let x2 = get_x2 n d in

    Array.map (fun x -> Complex.mul { re = 0.; im = tau /. 2. } x) wxx
    |> (Array.map2 Complex.add)
         (Array.map
            (fun y -> { Complex.re = 0.; im = -1. *. y *. tau /. 2. })
            x2)
    |> (Array.map2 Complex.add) w

  let step_mutate w tau b d n =
    let wxx = second_derivative w b d n in
    let x2 = get_x2 n d in
    for i = 0 to n - 1 do
      w.(i) <-
        Complex.add
          (Complex.mul { re = 0.; im = tau /. 2. } wxx.(i))
          w.(i)
        |> Complex.add
             { Complex.re = 0.; im = -1. *. x2.(i) *. tau /. 2. }
    done

  let evolve w tau b d time print =
    let n = Array.length w in
    let wnew = Array.copy w in
    let steps = Float.to_int (Float.ceil (time /. tau)) in
    for i = 0 to steps do
      (*TODO: Implement printing*)
      step_mutate wnew tau b d n
    done;

    wnew
end

type domain2d = domain * domain

module type Evolution2D = sig
  (** Representation type of function *)
  type t

  (** Represents maximum timestep value *)
  val dt : float

  (** List to represent the different allowed kinds of boundary
      conditions. *)
  val boundary_condition : boundary_conditions list

  (** Function to normalize a given vector representation of a wave
      function *)
  val normalize : Complex.t list list -> Complex.t list list

  (** Function to convert from vector representation of wave function to
      representation type*)
  val from_list : Complex.t list list -> t

  (** Function to convert from representation type to vector
      representation of wave function*)
  val to_list : t -> Complex.t list list

  (** Function to convert from representation type to vector
      representation of probability distribution (Psi^dag * Psi) *)
  val probabilities : t -> float list list

  (** Function to evolve a wavefunction in time for a single timestep.
      Returns the internal representation of the wavefunction after one
      timestep*)
  val step : t -> float -> boundary_conditions -> domain2d -> t

  (** Function to evolve a wavefunction in time for a given time (in
      seconds). Returns the internal representation of the wavefunction
      after the last timestep. Boolean argument tells function whether
      to print each timestep in a file. *)
  val evolve :
    t -> float -> boundary_conditions -> domain2d -> float -> bool -> t
end

(** [transpose mat] returns the matrix transpose of the 2d list mat *)
let rec transpose mat =
  match mat with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss)
      :: transpose (xs :: List.map List.tl xss)

(** [col_map f mat] returns mat with each column of mat replaced with
  the function f applied to that column. f therefore must take in an 'a list
  and returns a 'b list of the same length. *)
let rec col_map f mat = 
  match mat with
  |[] -> []
  |h :: t -> f h :: col_map f t

(** [row_map f mat] returns mat with each row of mat replaced with
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

(** Inverse fast fourier transform of the 2d list mat, with each
    dimension having n elements. Precondition: n is a power of 2 *)
let ifft2 mat n m =
  mat |> List.rev
  |> col_map (re_ifft m)
  |> List.rev |> transpose |> List.rev |> transpose
  |> row_map (re_ifft n)
  |> transpose |> List.rev |> transpose

let rec build_k2 lst1 lst2 = 
  match lst1 with
  |[] -> []
  |h :: t -> (List.map (fun x -> x +. h) lst2) :: (build_k2 t lst2)

let get_k2_2d (n : int) (m : int) (d2 : domain2d) =
  build_k2 (get_k2 n (fst d2)) (get_k2 m (snd d2))

let probabilites1d vec = vec |> List.map Complex.norm2

let rec probs mat =
  match mat with [] -> [] | h :: t -> probabilites1d h :: probs t

module FreeParticleEvolutionSpectral2D : Evolution2D = struct
  (** AF: t represents the 2D (discrete) fourier transform of a wave
      function. *)
  type t = Complex.t list list

  let dt = 0.01

  let boundary_condition = [ Periodic ]

  let norm1d vec =
    List.fold_left (fun acc x -> acc +. Complex.norm2 x) 0. vec |> sqrt

  let div1d vec norm =
    List.map (fun x -> Complex.div x { re = norm; im = 0. }) vec

  let normalize mat =
    let rec norm mat =
      match mat with [] -> 0. | h :: t -> norm1d h +. norm t
    in
    let matnorm = norm mat in
    let rec div mat =
      match mat with [] -> [] | h :: t -> div1d h matnorm :: div t
    in
    div mat

  (** [from_list mat] returns internal representation of mat, of type t.
      Precondition: mat is a Complex.t list list of length n by m, where
      n and m are both powers of 2.*)
  let from_list mat =
    (mat |> normalize |> fft2)
      (List.length mat)
      (List.length (List.hd mat))

  let to_list w = (w |> ifft2) (List.length w) (List.length (List.hd w))

  let probabilities w = 
    w |> to_list |> probs

  let rec mat_map f mat1 = 
    match mat1 with
    |[] -> []
    |h :: t -> (List.map f h) :: (mat_map f t)
  
  let rec mat_map2 f mat1 mat2 = 
    match mat1 with
    |[] -> []
    |h :: t -> 
      match mat2 with
      |[] -> []
      |h2 :: t2 -> 
        (List.map2 f h h2) :: (mat_map2 f t t2)

  let step w tau b d2 = 
    if b <> Periodic then 
      raise (Invalid_argument "Illegal boundary condition.");
    let n = List.length w in
    let m = List.length (List.hd w) in
    let k2 = get_k2_2d n m d2 in
    let mapped_k2 = 
      mat_map (fun x -> Complex.exp {Complex.re = 0.; im = -0.5 *. tau *. x}) k2
    in
    mat_map2 Complex.mul w mapped_k2

  let evolve w tau b d2 time print = step w time b d2

  (*TODO: Implement printing *)
end

let second_derivative_2d
    (w : Complex.t array array)
    (b : boundary_conditions)
    (d2 : domain2d)
    (n : int)
    (m : int)
    (is_x : bool) =
  if is_x then (
    let l = match fst d2 with a, b -> b -. a in
    let dx = l /. Float.of_int n in
    let wxx = Array.make n (Array.make m Complex.zero) in

    let boundary b wxx =
      match b with
      | Periodic ->
          for j = 0 to m - 1 do
            wxx.(0).(j) <-
              Complex.div
                (Complex.add w.(1).(j) (Complex.neg w.(n - 1).(j)))
                { re = 2. *. dx; im = 0. };
            wxx.(n - 1).(j) <-
              Complex.div
                (Complex.add w.(0).(j) (Complex.neg w.(n - 2).(j)))
                { re = 2. *. dx; im = 0. }
          done
      | Dirichlet -> ()
      | _ -> raise (Invalid_argument "Illegal boundary condition.")
    in
    boundary b wxx;

    for i = 1 to n - 2 do
      for j = 1 to m - 2 do
        wxx.(i).(j) <-
          Complex.div
            (Complex.add w.(i + 1).(j) (Complex.neg w.(i - 1).(j)))
            { re = 2. *. dx; im = 0. }
      done
    done;

    wxx)
  else
    let l = match snd d2 with a, b -> b -. a in
    let dy = l /. Float.of_int m in
    let wyy = Array.make n (Array.make m Complex.zero) in

    let boundary b wyy =
      match b with
      | Periodic ->
          for i = 0 to n - 1 do
            wyy.(i).(0) <-
              Complex.div
                (Complex.add w.(i).(1) (Complex.neg w.(i).(m - 1)))
                { re = 2. *. dy; im = 0. };
            wyy.(i).(m - 1) <-
              Complex.div
                (Complex.add w.(i).(0) (Complex.neg w.(i).(m - 2)))
                { re = 2. *. dy; im = 0. }
          done
      | Dirichlet -> ()
      | _ -> raise (Invalid_argument "Illegal boundary condition.")
    in
    boundary b wyy;

    for i = 1 to n - 2 do
      for j = 1 to m - 2 do
        wyy.(i).(j) <-
          Complex.div
            (Complex.add w.(i).(j + 1) (Complex.neg w.(i).(j - 1)))
            { re = 2. *. dy; im = 0. }
      done
    done;

    wyy

let der_mapper der tau n m =
  let mapped_der = Array.make n (Array.make m Complex.zero) in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      mapped_der.(i).(j) <-
        Complex.mul { re = 0.; im = tau /. 2. } der.(i).(j)
    done
  done;
  mapped_der

let square_mapper x2 tau =
  let n = Array.length x2 in
  let mapped_x2 = Array.make n Complex.zero in
  for i = 0 to n - 1 do
    mapped_x2.(i) <-
      { Complex.re = 0.; im = -1. *. x2.(i) *. tau /. 2. }
  done;
  mapped_x2

let adder arr1 arr2 n m =
  let res = Array.make n (Array.make m Complex.zero) in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      res.(i).(j) <- Complex.add arr1.(i).(j) arr2.(i).(j)
    done
  done;
  res

let arr2d_to_lst2d arr =
  let rlst = ref [] in
  let rcol = ref [] in
  for i = 0 to Array.length arr - 1 do
    for j = 0 to Array.length arr.(i) - 1 do
      rcol := arr.(i).(j) :: !rcol
    done;
    rlst := List.rev !rcol :: !rlst;
    rcol := []
  done;
  List.rev !rlst

let lst2d_to_arr2d mat =
  let arr =
    Array.make_matrix (List.length mat)
      (List.length (List.hd mat))
      Complex.zero
  in
  let rec col_builder arr col i j =
    match col with
    | [] -> ()
    | h :: t ->
        arr.(i).(j) <- h;
        col_builder arr t i (j + 1)
  in
  let rec builder arr mat i =
    match mat with
    | [] -> ()
    | h :: t ->
        col_builder arr h i 0;
        builder arr t (i + 1)
  in
  builder arr (FreeParticleEvolutionSpectral2D.normalize mat) 0;
  arr

module FreeParticleEvolutionEulers2D : Evolution2D = struct
  type t = Complex.t array array

  let dt = 0.0001

  let boundary_condition = [ Periodic; Dirichlet ]

  let normalize = FreeParticleEvolutionSpectral2D.normalize

  let from_list = lst2d_to_arr2d

  let to_list w = arr2d_to_lst2d w |> normalize

  let probabilities w = w |> to_list |> probs

  let step w tau b d2 =
    let n = Array.length w in
    let m = Array.length w.(0) in
    let wxx = second_derivative_2d w b d2 n m true in
    let wyy = second_derivative_2d w b d2 n m false in
    let mapped_wxx = der_mapper wxx tau n m in
    let mapped_wyy = der_mapper wyy tau n m in
    adder (adder mapped_wxx mapped_wyy n m) w n m

  let step_mutate w tau b d2 n m =
    let wxx = second_derivative_2d w b d2 n m true in
    let wyy = second_derivative_2d w b d2 n m false in
    let mapped_wxx = der_mapper wxx tau n m in
    let mapped_wyy = der_mapper wyy tau n m in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        w.(i).(j) <-
          Complex.add
            (Complex.add w.(i).(j) mapped_wxx.(i).(j))
            mapped_wyy.(i).(j)
      done
    done

  let evolve w tau b d2 time print =
    let n = Array.length w in
    let m = Array.length w.(0) in
    let wnew = Array.copy w in
    let steps = Float.to_int (Float.ceil (time /. tau)) in
    for i = 0 to steps do
      (*TODO: Implement printing*)
      step_mutate wnew tau b d2 n m
    done;

    wnew
end

module HarmonicOscillatorEvolutionEulers2D : Evolution2D = struct
  type t = Complex.t array array

  let dt = 0.0001

  let boundary_condition =
    FreeParticleEvolutionEulers2D.boundary_condition

  let normalize = FreeParticleEvolutionSpectral2D.normalize

  let from_list = lst2d_to_arr2d

  let to_list w = arr2d_to_lst2d w |> normalize

  let probabilities w = w |> to_list |> probs

  let step w tau b d2 =
    let n = Array.length w in
    let m = Array.length w.(0) in
    let wxx = second_derivative_2d w b d2 n m true in
    let wyy = second_derivative_2d w b d2 n m false in
    let x2 = get_x2 n (fst d2) in
    let y2 = get_x2 m (snd d2) in
    let mapped_wxx = der_mapper wxx tau n m in
    let mapped_wyy = der_mapper wyy tau n m in
    let mapped_x2 = square_mapper x2 tau in
    let mapped_y2 = square_mapper y2 tau in

    let res = adder (adder mapped_wxx mapped_wyy n m) w n m in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        res.(i).(j) <-
          Complex.add
            (Complex.add res.(i).(j) mapped_x2.(i))
            mapped_y2.(j)
      done
    done;

    res

  let step_mutate w tau b d2 n m =
    let wxx = second_derivative_2d w b d2 n m true in
    let wyy = second_derivative_2d w b d2 n m false in
    let x2 = get_x2 n (fst d2) in
    let y2 = get_x2 m (snd d2) in
    let mapped_wxx = der_mapper wxx tau n m in
    let mapped_wyy = der_mapper wyy tau n m in
    let mapped_x2 = square_mapper x2 tau in
    let mapped_y2 = square_mapper y2 tau in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        w.(i).(j) <-
          Complex.add
            (Complex.add
               (Complex.add
                  (Complex.add w.(i).(j) mapped_wxx.(i).(j))
                  mapped_wyy.(i).(j))
               mapped_x2.(i))
            mapped_y2.(j)
      done
    done

  let evolve w tau b d2 time print =
    let n = Array.length w in
    let m = Array.length w.(0) in
    let wnew = Array.copy w in
    let steps = Float.to_int (Float.ceil (time /. tau)) in
    for i = 0 to steps do
      (*TODO: Implement printing*)
      step_mutate wnew tau b d2 n m
    done;

    wnew
end
