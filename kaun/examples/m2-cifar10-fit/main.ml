(*---------------------------------------------------------------------------
  Copyright (c) 2026 The Raven authors. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* CIFAR-10 example using Kaun Training.fit and logging for kaun-console. *)
open Kaun

let () =
  (* Configuration *)
  let epochs = 15 in
  let batch_size = 64 in
  let learning_rate = 0.001 in

  let dtype = Rune.float32 in
  let rngs = Rune.Rng.key 0 in

  (* Create logger for monitoring *)
  let logger =
    Log.create ~experiment:"cifar10"
      ~config:
        [
          ("epochs", Jsont.Json.int epochs);
          ("batch_size", Jsont.Json.int batch_size);
          ("learning_rate", Jsont.Json.number learning_rate);
        ]
      ()
  in

  (* CNN for CIFAR-10: 32x32x3 -> conv 3->8 -> pool -> conv 8->16 -> pool -> 8*8*16=1024 -> dense -> 10 *)
  let model =
    Layer.sequential
      [
        Layer.conv2d ~in_channels:3 ~out_channels:8 ();
        Layer.relu ();
        Layer.avg_pool2d ~kernel_size:(2, 2) ();
        Layer.conv2d ~in_channels:8 ~out_channels:16 ();
        Layer.relu ();
        Layer.avg_pool2d ~kernel_size:(2, 2) ();
        Layer.flatten ();
        Layer.linear ~in_features:(8 * 8 * 16) ~out_features:128 ();
        Layer.relu ();
        Layer.linear ~in_features:128 ~out_features:10 ();
      ]
  in

  (* Metrics *)
  let metrics =
    Metrics.Collection.create
      [ ("loss", Metrics.loss ()); ("accuracy", Metrics.accuracy ()) ]
  in

  (* Default ~normalize:true gives NCHW + ImageNet normalization from kaun_datasets *)
  let train_data =
    Kaun_datasets.cifar10 ~train:true ()
    |> Dataset.prepare ~shuffle_buffer:50000 ~batch_size ~prefetch:2
  in

  let test_data =
    Kaun_datasets.cifar10 ~train:false ()
    |> Dataset.prepare ~batch_size:100 ~prefetch:2
  in

  Printf.printf "Starting CIFAR-10 training...\n";
  Printf.printf "Run ID: %s\n" (Log.run_id logger);
  Printf.printf "Run directory: %s\n" (Log.run_dir logger);
  Printf.printf "\n";
  Printf.printf "To monitor this run, open another terminal and run:\n";
  Printf.printf "  dune exec kaun-console\n";
  Printf.printf "\n%!";

  let _state, _history =
    let lr = Optimizer.Schedule.constant learning_rate in
    Training.fit ~model ~optimizer:(Optimizer.adam ~lr ())
      ~loss_fn:Loss.softmax_cross_entropy_with_indices ~metrics ~train_data
      ~val_data:test_data ~epochs ~progress:false
      ~callbacks:[ Training.Callbacks.logging logger ]
      ~rngs ~dtype ()
  in

  Log.close logger
