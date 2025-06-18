open Alcotest
open Nx_datasets

let check_shape name expected_shape tensor =
  let actual_shape = Nx.shape tensor in
  check (array int) name expected_shape actual_shape

let test_make_blobs () =
  let x, y = make_blobs ~n_samples:100 ~n_features:2 ~centers:(`N 3) () in
  check_shape "X shape" [| 100; 2 |] x;
  check_shape "y shape" [| 100 |] y

let test_make_classification () =
  let x, y =
    make_classification ~n_samples:100 ~n_features:20 ~n_classes:3 ()
  in
  check_shape "X shape" [| 100; 20 |] x;
  check_shape "y shape" [| 100 |] y

let test_make_gaussian_quantiles () =
  let x, y = make_gaussian_quantiles ~n_samples:100 ~n_features:2 () in
  check_shape "X shape" [| 100; 2 |] x;
  check_shape "y shape" [| 100 |] y

let test_make_circles () =
  let x, y = make_circles ~n_samples:100 () in
  check_shape "X shape" [| 100; 2 |] x;
  check_shape "y shape" [| 100 |] y

let test_make_moons () =
  let x, y = make_moons ~n_samples:100 () in
  check_shape "X shape" [| 100; 2 |] x;
  check_shape "y shape" [| 100 |] y

let test_make_regression () =
  let x, y, coef =
    make_regression ~n_samples:100 ~n_features:10 ~coef:true ()
  in
  check_shape "X shape" [| 100; 10 |] x;
  check_shape "y shape" [| 100; 1 |] y;
  match coef with
  | Some c -> check_shape "coef shape" [| 10; 1 |] c
  | None -> fail "Expected coefficients"

let test_make_friedman1 () =
  let x, y = make_friedman1 ~n_samples:100 () in
  check_shape "X shape" [| 100; 10 |] x;
  check_shape "y shape" [| 100 |] y

let test_make_s_curve () =
  let x, t = make_s_curve ~n_samples:100 () in
  check_shape "X shape" [| 100; 3 |] x;
  check_shape "t shape" [| 100 |] t

let test_make_swiss_roll () =
  let x, t = make_swiss_roll ~n_samples:100 () in
  check_shape "X shape" [| 100; 3 |] x;
  check_shape "t shape" [| 100 |] t

let test_make_low_rank_matrix () =
  let x = make_low_rank_matrix ~n_samples:50 ~n_features:100 () in
  check_shape "X shape" [| 50; 100 |] x

let test_make_spd_matrix () =
  let x = make_spd_matrix ~n_dim:30 () in
  check_shape "X shape" [| 30; 30 |] x

let test_make_biclusters () =
  let x, row_labels, col_labels = make_biclusters () in
  check_shape "X shape" [| 100; 100 |] x;
  check_shape "row_labels shape" [| 100 |] row_labels;
  check_shape "col_labels shape" [| 100 |] col_labels

let () =
  run "Generators"
    [
      ( "Classification",
        [
          test_case "make_blobs" `Quick test_make_blobs;
          test_case "make_classification" `Quick test_make_classification;
          test_case "make_gaussian_quantiles" `Quick
            test_make_gaussian_quantiles;
          test_case "make_circles" `Quick test_make_circles;
          test_case "make_moons" `Quick test_make_moons;
        ] );
      ( "Regression",
        [
          test_case "make_regression" `Quick test_make_regression;
          test_case "make_friedman1" `Quick test_make_friedman1;
        ] );
      ( "Manifold",
        [
          test_case "make_s_curve" `Quick test_make_s_curve;
          test_case "make_swiss_roll" `Quick test_make_swiss_roll;
        ] );
      ( "Matrix",
        [
          test_case "make_low_rank_matrix" `Quick test_make_low_rank_matrix;
          test_case "make_spd_matrix" `Quick test_make_spd_matrix;
          test_case "make_biclusters" `Quick test_make_biclusters;
        ] );
    ]
