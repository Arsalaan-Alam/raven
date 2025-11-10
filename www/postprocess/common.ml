open Soup

let parse_html_file path =
  let stream, close = Markup.file path in
  let signals =
    stream |> Markup.parse_html ~context:`Document |> Markup.signals
  in
  let soup = Soup.from_signals signals in
  close ();
  soup

let create_wrapper () = Soup.create_element "div" ~class_:"api-content"

let extract_content soup =
  match soup $? "body" with
  | None -> failwith "odoc file is missing <body>"
  | Some body ->
      let header = body $? "header.odoc-preamble" in
      let content_div = body $? "div.odoc-content" in
      let wrapper = create_wrapper () in
      (match header with Some h -> append_child wrapper h | None -> ());
      (match content_div with Some c -> append_child wrapper c | None -> ());
      wrapper

let remove_odoc_chrome node =
  node $$ "nav.odoc-nav" |> iter delete;
  node $$ "nav.odoc-toc" |> iter delete;
  node $$ "div.odoc-search" |> iter delete;
  node $$ "script" |> iter delete;
  node $$ "link[rel=stylesheet]" |> iter delete;
  node $$ "div.indextable" |> iter delete

let class_mappings =
  [
    ("odoc", "odoc-root");
    ("odoc-doc", "doc-content");
    ("odoc-content", "api-sections");
    ("odoc-spec", "api-spec");
    ("odoc-type", "api-type");
    ("odoc-val", "api-value");
    ("odoc-module", "api-module");
    ("odoc-include", "api-include");
    ("odoc-class", "api-class");
  ]

let remap_classes node =
  List.iter
    (fun (old_class, new_class) ->
      node $$ "." ^ old_class
      |> iter (fun elem ->
             remove_class old_class elem;
             add_class new_class elem))
    class_mappings

let highlight_code node =
  node $$ "pre code"
  |> iter (fun code ->
         add_class "language-ocaml" code;
         match parent code with
         | Some pre -> add_class "code-block" pre
         | None -> ());
  node $$ "code"
  |> iter (fun code ->
         match parent code with
         | Some p when name p <> "pre" -> add_class "inline-code" code
         | _ -> ())

let add_heading_style node =
  node $$ "h1" |> iter (fun h -> add_class "with-backing" h);
  node $$ "h2" |> iter (fun h -> add_class "with-backing" h);
  node $$ "h3" |> iter (fun h -> add_class "with-backing" h);
  node $$ ".api-spec"
  |> iter (fun spec -> add_class "spec-backing" spec)

let remove_stdlib_prefix node =
  let re = Str.regexp "\\bStdlib\\." in
  node |> descendants |> elements
  |> iter (fun elem ->
         let text_content = texts elem |> String.concat "" in
         if String.length text_content > 0 then
           let replaced = Str.global_replace re "" text_content in
           if replaced <> text_content then (
             clear elem;
             append_child elem (Soup.create_text replaced)))

let extract_title content =
  match content $? "h1" with
  | None -> "API Reference"
  | Some h1 ->
      let gather_text acc child =
        match element child with
        | None -> acc ^ to_string child
        | Some el when name el = "a" -> acc
        | Some el -> acc ^ (texts el |> String.concat "")
      in
      children h1 |> to_list |> List.fold_left gather_text "" |> String.trim

let is_directory path =
  try (Unix.stat path).st_kind = Unix.S_DIR with Unix.Unix_error _ -> false

let rec remove_recursively path =
  if Sys.file_exists path || is_directory path then
    if is_directory path then (
      Sys.readdir path
      |> Array.iter (fun entry ->
             let child = Filename.concat path entry in
             remove_recursively child);
      Unix.rmdir path)
    else Sys.remove path

let rec ensure_dir path =
  if path = "." || path = Filename.parent_dir_name || path = "/" then ()
  else if Sys.file_exists path then ()
  else (
    ensure_dir (Filename.dirname path);
    if not (Sys.file_exists path) then Unix.mkdir path 0o755)

let ensure_parent_directory path =
  let dir = Filename.dirname path in
  ensure_dir dir

let write_file path content =
  ensure_parent_directory path;
  let oc = open_out path in
  output_string oc content;
  close_out oc

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let normalise_relative_path path =
  let parts =
    path |> String.split_on_char '/' |> List.filter (fun s -> s <> "")
  in
  let rec collapse acc = function
    | [] -> List.rev acc
    | "." :: rest -> collapse acc rest
    | ".." :: rest -> collapse (match acc with [] -> [] | _ :: tl -> tl) rest
    | p :: rest -> collapse (p :: acc) rest
  in
  collapse [] parts |> String.concat "/"

