type nav_item = {
  title : string;
  href : string;
  depth : int;
}

let html_extension = ".html"

let is_html_file filename = Filename.check_suffix filename html_extension

let relativise ~root path =
  let prefix = String.length root + 1 in
  String.sub path prefix (String.length path - prefix)

let collect_html_files root =
  let rec walk acc dir =
    Sys.readdir dir
    |> Array.fold_left
         (fun acc entry ->
           let path = Filename.concat dir entry in
           if Sys.is_directory path then walk acc path
           else if is_html_file entry then path :: acc
           else acc)
         acc
  in
  walk [] root

let should_list_in_nav relative_path =
  let components = String.split_on_char '/' relative_path in
  match List.rev components with
  | [] -> false
  | filename :: rest -> (
      let depth = List.length rest in
      match filename with
      | "index.html" -> depth <= 1
      | _ -> depth = 0)

let build_nav_item ~library relative_path title =
  let depth =
    let components =
      String.split_on_char '/' relative_path |> List.filter (( <> ) "")
    in
    match components with
    | [] -> 0
    | _ :: rest -> List.length rest
  in
  {
    title;
    depth;
    href =
      "/docs/" ^ library ^ "/api/"
      ^ Common.normalise_relative_path relative_path;
  }

let render_nav items =
  let buffer = Buffer.create 256 in
  List.iter
    (fun item ->
      let indent = String.make (item.depth * 2) ' ' in
      Buffer.add_string buffer indent;
      Buffer.add_string buffer "<li>";
      Buffer.add_string buffer
        (Printf.sprintf "<a href=\"%s\">%s</a>" item.href item.title);
      Buffer.add_string buffer "</li>\n")
    items;
  Buffer.contents buffer

let prepare_destination ~output_dir ~nav_output =
  if Sys.file_exists output_dir then Common.remove_recursively output_dir;
  Common.ensure_dir output_dir;
  if Sys.file_exists nav_output then Common.remove_recursively nav_output;
  Common.ensure_parent_directory nav_output

let process_file ~root ~output relative_path =
  let source = Filename.concat root relative_path in
  let soup = Common.parse_html_file source in
  let content = Common.extract_content soup in
  Common.remove_odoc_chrome content;
  Common.remap_classes content;
  Common.highlight_code content;
  Common.add_heading_style content;
  let title = Common.extract_title content in
  let html = Soup.to_string content in
  let destination = Filename.concat output relative_path in
  Common.write_file destination html;
  title

let copy_static ~root ~library ~output () =
  let assets = [ "db.js" ] in
  List.iter
    (fun asset ->
      let source = Filename.concat root asset in
      if Sys.file_exists source then
        let destination = Filename.concat output asset in
        Common.write_file destination (Common.read_file source))
    assets;
  let sherlodoc = Filename.concat (Filename.dirname root) "sherlodoc.js" in
  if Sys.file_exists sherlodoc then
    let destination =
      Filename.concat output (library ^ "_sherlodoc.js")
    in
    Common.write_file destination (Common.read_file sherlodoc)

let () =
  match Sys.argv with
  | [| _; library; input_dir; output_dir; nav_output |] ->
      prepare_destination ~output_dir ~nav_output;
      let html_files =
        collect_html_files input_dir
        |> List.map (relativise ~root:input_dir)
        |> List.sort String.compare
      in
      let nav_items =
        List.filter_map
          (fun relative_path ->
            let title =
              process_file ~root:input_dir ~output:output_dir relative_path
            in
            if should_list_in_nav relative_path then
              Some (build_nav_item ~library relative_path title)
            else None)
          html_files
      in
      let nav_html = render_nav nav_items in
      Common.write_file nav_output nav_html;
      copy_static ~root:input_dir ~library ~output:output_dir ()
  | _ ->
      prerr_endline
        "usage: index.exe <library> <input_dir> <output_dir> <nav_output>";
      exit 1

