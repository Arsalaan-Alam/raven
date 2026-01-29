(*---------------------------------------------------------------------------
  Copyright (c) 2026 The Raven authors. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Mosaic
open Kaun_filesystem
module Charts = Matrix_charts

(* ───── Model ───── *)

type model = {
  run_id : string;
  store : Metric_store.t;
  reader : Event_reader.t;
}

type msg = Tick of float | Quit

(* ───── Constants ───── *)

let header_bg = Ansi.Color.of_rgb 30 80 100
let hint_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let step_color = Ansi.Color.cyan
let epoch_color = Ansi.Color.cyan

(* ───── View Components ───── *)

let view_header ~run_id store =
  box ~padding:(padding 1) ~background:header_bg
    ~size:{ width = pct 100; height = auto }
    [
      box ~flex_direction:Row ~gap:(gap 2) ~align_items:Center
        ~size:{ width = pct 100; height = auto }
        [
          text ~style:(Ansi.Style.make ~bold:true ()) "▸ Kaun Console";
          text
            ~style:(Ansi.Style.make ~fg:step_color ())
            (Printf.sprintf "Run: %s" run_id);
          (match Metric_store.latest_epoch store with
          | None -> text ~style:hint_style "Epoch: -"
          | Some e ->
              text
                ~style:(Ansi.Style.make ~fg:epoch_color ())
                (Printf.sprintf "Epoch: %d" e));
          box ~flex_grow:1.0 ~size:{ width = auto; height = auto } [];
          box ~padding:(padding 1) ~background:Ansi.Color.green
            [
              text
                ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.white ())
                "LIVE"
            ];
        ];
    ]

(* ───── Chart Drawing ───── *)

let axis_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:12) ~dim:true ()
let y_axis_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:12) ~dim:true ()
let grid_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:6) ~dim:true ()

let draw_metric_chart _tag history grid ~width ~height =
  if history = [] then
    (* No data yet - show placeholder *)
    ()
  else
    (* Convert (step, value) list to array of (x, y) tuples *)
    let data = Array.of_list (List.map (fun (step, value) -> (float_of_int step, value)) history) in
    let chart =
      Charts.empty ()
      |> Charts.with_frame (Charts.manual_frame ~margins:(1, 0, 0, 2) ())
      |> Charts.with_axes
           ~x:
             (Charts.Axis.default |> Charts.Axis.with_ticks 4
             |> Charts.Axis.with_style axis_style)
           ~y:
             (Charts.Axis.default |> Charts.Axis.with_ticks 2
             |> Charts.Axis.with_style y_axis_style
             |> Charts.Axis.with_format (fun _ v -> Printf.sprintf "%.1f" v))
      |> Charts.with_grid
           (Charts.Gridlines.default
           |> Charts.Gridlines.with_style grid_style
           |> Charts.Gridlines.with_x true
           |> Charts.Gridlines.with_y true)
      |> Charts.line ~resolution:`Braille2x4
           ~style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
           ~x:fst ~y:snd data
    in
    ignore (Charts.draw chart grid ~width ~height)

let view_metric_chart store tag (_m : Metric_store.metric) =
  let history = Metric_store.history_for_tag store tag in
  box ~border:true ~title:tag ~padding:(padding 0)
    ~size:{ width = pct 100; height = px 14 }
    [
      canvas
        ~draw:(fun grid ~width ~height -> draw_metric_chart tag history grid ~width ~height)
        ~size:{ width = pct 100; height = pct 100 }
        ();
    ]

let view_metrics store =
  let latest = Metric_store.latest_metrics store in
  if latest = [] then
    box ~padding:(padding 1)
      [ text ~style:hint_style "  Waiting for metrics..." ]
  else
    box ~flex_direction:Column ~padding:(padding 1) ~gap:(gap 1)
      [

        box ~flex_direction:Column ~gap:(gap 1)
          (List.map
             (fun (tag, m) -> view_metric_chart store tag m)
             latest);
      ]

let view_imp_info () =
  box ~padding:(padding 1)
    [ text ~style:(Ansi.Style.make ~bold:true ()) "imp info" ]

let view_sys_panel () =
  box ~padding:(padding 1)
    [ text ~style:(Ansi.Style.make ~bold:true ()) "sys panel" ]

let view_footer () =
  box ~padding:(padding 1)
    [ text ~style:hint_style "(Press Ctrl-C to quit)" ]

let view m =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      view_header ~run_id:m.run_id m.store;

      box ~flex_direction:Row ~flex_grow:1.0
        ~size:{ width = pct 100; height = pct 100 }
        [
          (* Left column: imp info *)
          scroll_box ~scroll_y:true ~scroll_x:false
            ~size:{ width = pct 33; height = pct 100 }
            [ view_imp_info () ];
          (* Vertical divider *)
          box
            ~size:{ width = px 1; height = pct 100 }
            ~background:(Ansi.Color.grayscale ~level:8)
            [ text " " ];
          (* Middle column: metrics *)
          scroll_box ~scroll_y:true ~scroll_x:false
            ~size:{ width = pct 34; height = pct 100 }
            [ view_metrics m.store ];
          (* Vertical divider *)
          box
            ~size:{ width = px 1; height = pct 100 }
            ~background:(Ansi.Color.grayscale ~level:8)
            [ text " " ];
          (* Right column: sys panel *)
          scroll_box ~scroll_y:true ~scroll_x:false
            ~size:{ width = pct 33; height = pct 100 }
            [ view_sys_panel () ];
        ];
      view_footer ();
    ]

(* ───── TEA Core ───── *)

let init ~run_id ~events_path =
  let reader = Event_reader.create ~file_path:events_path in
  let store = Metric_store.create () in
  (* Load all existing events on startup to build history *)
  Event_reader.reset reader;
  let rec load_all_events acc =
    match Event_reader.read_new reader with
    | [] -> acc
    | new_events -> load_all_events (acc @ new_events)
  in
  let all_events = load_all_events [] in
  Metric_store.update store all_events;
  ({ run_id; store; reader }, Cmd.none)

let update msg m =
  match msg with
  | Tick _ ->
      let new_events = Event_reader.read_new m.reader in
      Metric_store.update m.store new_events;
      ({ m with store = m.store }, Cmd.none)
  | Quit ->
      Event_reader.close m.reader;
      (m, Cmd.quit)

let subscriptions _model =
  Sub.batch
    [
      Sub.on_tick (fun ~dt -> Tick dt);
      Sub.on_key (fun ev ->
          match (Mosaic_ui.Event.Key.data ev).key with
          | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
          | Char c when Uchar.equal c (Uchar.of_char 'Q') -> Some Quit
          | Escape -> Some Quit
          | _ -> None);
    ]

let run ?(base_dir = "./runs") ?experiment:_ ?tags:_ ?runs () =
  match runs with
  | Some [ run_id ] ->
      let run_dir = Filename.concat base_dir run_id in
      let events_path = Manifest.events_path ~run_dir in

      let init () = init ~run_id ~events_path in
      Mosaic.run { init; update; view; subscriptions }
  | _ ->
   Printf.printf "kaun-console: please specify a single run\n%!"
