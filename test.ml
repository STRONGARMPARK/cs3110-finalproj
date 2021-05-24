open OUnit2
open Graphs
open Graphs2d
(*
Testing Plan:

The thing about our project, was that most of our application, and modules
were not testable in the traditional sense. They didn't return anything
that we could process. For example, the interface was completely unit-return
based, and the graphing modules similarly. Therefore, we could not test the 
majority of the GUI. However, in the process of 3d and 2d graphing, there 
were a few functions that included mathematical operations with testable 
return values.  

The modules evolution and userint were not tested because of reasons
specified above. However, all of Graph and Graph2d modules were tested using
the glass-box testing approach. 

For the correctness of the system, the only modules and parts of modules 
that needed testing for correctess, were inside of the mathematical 
operations that we used in Graph and Graph2d. Therefore, as we have 
tested all of these modules, the system should be tested and correct.*)

let print_v_int (v : int * int) =
  "( " ^ string_of_int (fst v) ^ " , " ^ string_of_int (snd v) ^ " )"

let print_v_float (v : float * float) =
  "( "
  ^ string_of_float (fst v)
  ^ " , "
  ^ string_of_float (snd v)
  ^ " )"

let test_vop name op a b expected printer =
  name >:: fun _ -> assert_equal expected (Graphs.vop op a b) ~printer

let test_vfloat name a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs.vfloat a) ~printer:print_v_float

let test_vint name a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs.vint a) ~printer:print_v_int

let test_mag_float name a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs.mag_float a) ~printer:(fun x ->
      string_of_float x)

let test_mag_int name a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs.mag_int a) ~printer:(fun x ->
      string_of_float x)

let graphs_tests =
  [
    test_vop "2d point addition operation with ints" ( + ) (1, 1) (0, 2)
      (1, 3) print_v_int;
    test_vop "2d point addition operation with floats" ( +. ) (4., 2.)
      (2., 3.) (6., 5.) print_v_float;
    test_vop "2d point addition operation with ints" ( * ) (1, 1) (0, 2)
      (0, 2) print_v_int;
    test_vop "2d point addition operation with floats" ( *. ) (4., 2.)
      (2., 3.) (8., 6.) print_v_float;
    test_vfloat "2d point int to float" (2, 2) (2., 2.);
    test_vint "2d point float to int" (2., 2.) (2, 2);
    test_mag_float "magnitude of 2d float point (4.,3.)" (4., 3.) 5.;
    test_mag_int "magnitude of 2d int point (4,3)" (4, 3) 5.;
  ]

(* Graphs2d TESTS *)

let print_v (v : Graphs2d.v) =
  let vals =
    List.fold_left (fun s x -> s ^ " ; " ^ string_of_float x) "" v
  in
  "[ " ^ vals ^ " ]"

let print_v3 (v3 : Graphs2d.v3) =
  "( " ^ string_of_float v3.x ^ " , " ^ string_of_float v3.y ^ " , "
  ^ string_of_float v3.z ^ " )"

let print_m (m : Graphs2d.m) =
  let vals = List.fold_left (fun s v -> s ^ " ; " ^ print_v v) "" m in
  "[ " ^ vals ^ " ]"

let print_box (b : Graphs2d.box) =
  "{ " ^ print_v3 b.v1 ^ ", " ^ print_v3 b.v2 ^ ", " ^ print_v3 b.v3
  ^ ", " ^ print_v3 b.v4 ^ ", " ^ print_v3 b.v5 ^ ", " ^ print_v3 b.v6
  ^ ", " ^ print_v3 b.v7 ^ ", " ^ print_v3 b.v8 ^ ", " ^ " }"

let print_c (c : Graphs2d.c) =
  "( r = " ^ string_of_float c.r ^ " , g = " ^ string_of_float c.g
  ^ " , b = " ^ string_of_float c.b ^ " )"

let test_int_to_c name i expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.int_to_c i) ~printer:print_c

let test_v3op_const name op b a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.v3op_const op b a) ~printer:print_v3

let test_v3op name op b a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.v3op op b a) ~printer:print_v3

let test_v3op_bool name op b a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.v3op_bool op b a) ~printer:(fun x ->
      string_of_bool x)

let test_v3op_bool_const name op b a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.v3op_bool_const op b a)
    ~printer:(fun x -> string_of_bool x)

let test_box_bool name op b a expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.box_bool op b a) ~printer:(fun x ->
      string_of_bool x)

let test_to_float name intm expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.to_float intm) ~printer:print_m

let test_v3_to_m name v3 expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.v3_to_m v3) ~printer:print_m

let test_m_to_v3 name m expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.m_to_v3 m) ~printer:print_v3

let test_m_to_v name m expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.m_to_v m) ~printer:print_v

let test_v3_to_v name v3 expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.v3_to_v v3) ~printer:print_v

let test_v_to_v3 name v expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.v_to_v3 v) ~printer:print_v3

let test_transpose name m expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.transpose m) ~printer:print_m

let test_dot name a b expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.dot a b) ~printer:(fun x ->
      string_of_float x)

let test_cross name a b expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.cross a b) ~printer:print_v3

let test_v_dot name a b expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.( ** ) a b) ~printer:(fun x ->
      string_of_float x)

let test_mat_mult name a b expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.( *** ) a b) ~printer:print_m

let test_mag name v3 expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.mag v3) ~printer:(fun x ->
      string_of_float x)

let test_norm name v3 expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.norm v3) ~printer:print_v3

let test_box name v1 v2 expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.box v1 v2) ~printer:print_box

let test_box_map name f box expected =
  name >:: fun _ ->
  assert_equal expected (Graphs2d.box_map f box) ~printer:print_box

let test_box_fold_left name f init box expected print =
  name >:: fun _ ->
  assert_equal expected
    (Graphs2d.box_fold_left f init box)
    ~printer:print

let test_box_closest name p box expected =
  name >:: fun _ ->
  assert_equal expected
    (Graphs2d.box_closest_v3 p box)
    ~printer:print_v3

let test_box_farthest name p box expected =
  name >:: fun _ ->
  assert_equal expected
    (Graphs2d.box_farthest_v3 p box)
    ~printer:print_v3

let b1 =
  Graphs2d.box { x = 0.; y = 0.; z = 0. } { x = 10.; y = 10.; z = 10. }

let b2 =
  Graphs2d.box
    { x = 0.; y = 0.; z = 0. }
    { x = -10.; y = -10.; z = -10. }

let graphs_2d_tests =
  [
    test_int_to_c "test int to c for color black 0" 0
      { r = 0.; g = 0.; b = 0. };
    test_int_to_c "test int to c for color white 16777215" 16777215
      { r = 255.; g = 255.; b = 255. };
    test_v3op_const
      "test point constant addition with 3d vector (3., 2., 4.) with \
       value 3."
      ( +. ) 3.
      { x = 3.; y = 2.; z = 4. }
      { x = 6.; y = 5.; z = 7. };
    test_v3op_const
      "test point constant multiplication with 3d vector (3., 2., 4.) \
       with value 3."
      ( *. ) 3.
      { x = 3.; y = 2.; z = 4. }
      { x = 9.; y = 6.; z = 12. };
    test_v3op
      "test vector addition with 3d vector (3., 2., 4.) with value \
       (3., 3., 3.)"
      ( +. )
      { x = 3.; y = 3.; z = 3. }
      { x = 3.; y = 2.; z = 4. }
      { x = 6.; y = 5.; z = 7. };
    test_v3op
      "test vector value multiplication with 3d vector (3., 2., 4.) \
       with value (3., 3., 3.)"
      ( *. )
      { x = 3.; y = 3.; z = 3. }
      { x = 3.; y = 2.; z = 4. }
      { x = 9.; y = 6.; z = 12. };
    test_v3op_bool
      "test vector boolean operator ( < ) with 3d vector (3., 2., 4.) \
       with value (2., 1., 3.)"
      ( < )
      { x = 2.; y = 1.; z = 3. }
      { x = 3.; y = 2.; z = 4. }
      true;
    test_v3op_bool
      "test vector boolean operator ( = ) with 3d vector (3., 2., 4.) \
       with value (2., 2., 3.)"
      ( = )
      { x = 2.; y = 2.; z = 3. }
      { x = 3.; y = 2.; z = 4. }
      false;
    test_v3op_bool_const
      "test vector boolean constant operator ( <= ) with 3d vector \
       (3., 2., 4.) with value 2."
      ( <= ) 2.
      { x = 3.; y = 2.; z = 4. }
      false;
    test_v3op_bool_const
      "test vector boolean constant operator ( > ) with 3d vector (3., \
       2., 4.) with value 2."
      ( <> ) 2.
      { x = 3.; y = 2.; z = 4. }
      false;
    test_box_bool
      "test box boolean operator (<>) on box b1 with corners (0,0,0) \
       and (10,10,10) and box b2 with corners (0,0,0) and \
       (-10,-10,-10)"
      ( <> ) b1 b2 true;
    test_box_bool
      "test box boolean operator ( = ) on box b1 with corners (0,0,0) \
       and (10,10,10) and box b2 with corners (0,0,0) and (10,10,10)"
      ( = ) b1 b1 true;
    test_to_float "test converting int matrix to float matrix"
      [ [ 1; 1 ]; [ 2; 2 ] ]
      [ [ 1.; 1. ]; [ 2.; 2. ] ];
    test_v3_to_m "test converting vector3 to float matrix"
      { x = 1.; y = 2.; z = 3. }
      [ [ 1. ]; [ 2. ]; [ 3. ] ];
    test_m_to_v3 "test converting 3x1 matrix to vector3"
      [ [ 1. ]; [ 2. ]; [ 3. ] ]
      { x = 1.; y = 2.; z = 3. };
    test_m_to_v3 "test converting 3x3 matrix to vector3"
      [ [ 1.; 4.; 5. ]; [ 2.; 4.; 6. ]; [ 3.; 1.; 1. ] ]
      { x = 1.; y = 2.; z = 3. };
    test_m_to_v3 "test converting 2x2 matrix to vector3"
      [ [ 1.; 2. ]; [ 2.; 4. ] ]
      { x = 1.; y = 2.; z = 0. };
    test_m_to_v3 "test converting 1x3 matrix to vector3"
      [ [ 1.; 2.; 3. ] ]
      { x = 1.; y = 0.; z = 0. };
    test_m_to_v "test converting 3x1 matrix to vector"
      [ [ 1. ]; [ 2. ]; [ 3. ] ]
      [ 1. ];
    test_m_to_v "test converting 3x3 matrix to vector"
      [ [ 1.; 4.; 5. ]; [ 2.; 4.; 6. ]; [ 3.; 1.; 1. ] ]
      [ 1.; 4.; 5. ];
    test_m_to_v "test converting 2x2 matrix to vector"
      [ [ 1.; 2. ]; [ 2.; 4. ] ]
      [ 1.; 2. ];
    test_m_to_v "test converting 1x3 matrix to vector"
      [ [ 1.; 2.; 3. ] ] [ 1.; 2.; 3. ];
    test_v3_to_v "test converting vector3 to vectorN"
      { x = 1.; y = 2.; z = 3. }
      [ 1.; 2.; 3. ];
    test_v_to_v3 "test converting vectorN of size 5 to vector3"
      [ 1.; 2.; 3.; 4.; 5. ]
      { x = 1.; y = 2.; z = 3. };
    test_v_to_v3 "test converting vectorN of size 3 to vector3"
      [ 1.; 2.; 3. ]
      { x = 1.; y = 2.; z = 3. };
    test_v_to_v3 "test converting vectorN of size 2 to vector3"
      [ 1.; 2. ]
      { x = 1.; y = 2.; z = 0. };
    test_v_to_v3 "test converting vectorN of size 1 to vector3" [ 1. ]
      { x = 1.; y = 0.; z = 0. };
    test_v_to_v3 "test converting vectorN of size 0 to vector3" []
      { x = 0.; y = 0.; z = 0. };
    test_transpose "test transpose on matrix of size 4x3 -> 3x4 matrix"
      [ [ 3.; 2.; 3. ]; [ 1.; 2.; 4. ]; [ 5.; 2.; 3. ]; [ 5.; 2.; 1. ] ]
      [ [ 3.; 1.; 5.; 5. ]; [ 2.; 2.; 2.; 2. ]; [ 3.; 4.; 3.; 1. ] ];
    test_transpose "test transpose on matrix of size 0x0 -> 0x0 matrix"
      [] [];
    test_transpose "test transpose on matrix of size 1x1 -> 1x1 matrix"
      [ [ 3. ] ] [ [ 3. ] ];
    test_dot "test dot product with two 3d vectors"
      { x = 1.; y = 2.; z = 3. }
      { x = 2.; y = 2.; z = 2. }
      12.;
    test_cross "test cross product with two 3d vectors"
      { x = 1.; y = 2.; z = 3. }
      { x = 2.; y = 2.; z = 2. }
      { x = -2.; y = 4.; z = -2. };
    test_v_dot "test dot product on two vectors of size 5"
      [ 3.; 2.; 3.; 5. ] [ 2.; 2.; 2.; 2. ] 26.;
    test_v_dot "test dot product on two vectors of different sizes"
      [ 3.; 2.; 3.; 5. ] [ 2.; 2.; 2.; 2. ] 26.;
    ( "test dot product on two vectors of different sizes" >:: fun _ ->
      assert_raises (Invalid_argument "List.fold_left2") (fun () ->
          Graphs2d.( ** ) [ 3.; 2.; 3.; 5. ] [ 2.; 2.; 2. ]) );
    test_mat_mult "test multiplying two matrices"
      [ [ 2.; 2. ]; [ 2.; 3. ] ]
      [ [ 2.; 2. ]; [ 2.; 3. ] ]
      [ [ 8.; 10. ]; [ 10.; 13. ] ];
    test_mag "test magnitude of vector 3 with value (3,4,5)"
      { x = 3.; y = 4.; z = 5. }
      (sqrt (9. +. 16. +. 25.));
    test_norm "test normalization of vector 3 with value (3,4,5)"
      { x = 3.; y = 4.; z = 5. }
      { x = 3. /. sqrt 50.; y = 4. /. sqrt 50.; z = 5. /. sqrt 50. };
    test_box "create a box with corner vectors (0,0,0) (10,10,10)"
      { x = 0.; y = 0.; z = 0. }
      { x = 10.; y = 10.; z = 10. }
      b1;
    test_box_map
      "map each vector of box with corner vectors (0,0,0) (10,10,10) \
       by multiplying each by 10."
      (fun v -> v3op_const ( *. ) 10. v)
      b1
      (box { x = 0.; y = 0.; z = 0. } { x = 100.; y = 100.; z = 100. });
    test_box_fold_left
      "fold left each vector of box with corner vectors (0,0,0) \
       (10,10,10) using a function that adds all the values together"
      (fun i v -> i +. v.x +. v.y +. v.z)
      0. b1 120.
      (fun x -> string_of_float x);
    test_box_closest
      "get the closeset vector of box with corner vectors (0,0,0) \
       (10,10,10) for point (40, 0, 40)"
      { x = 40.; y = 0.; z = 40. }
      b1
      { x = 10.; y = 10.; z = 10. };
    test_box_farthest
      "get the farthest vector of box with corner vectors (0,0,0) \
       (10,10,10) for point (40, 0, 40)"
      { x = 40.; y = 0.; z = 40. }
      b1
      { x = 0.; y = 0.; z = 0. };
  ]

let suite =
  "test suite for OCAML final proj"
  >::: List.flatten [ graphs_tests; graphs_2d_tests ]

let _ = run_test_tt_main suite
