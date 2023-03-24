(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* This module uses some code from the small configwin library (package lablgtk3-extras). *)

open Preferences

(** Types and functions to generate a dialog with tabs organized in a tree *)

type pref_section = {
  label: string;
  box: GPack.box;
  apply: unit -> unit;
}

type pref_category = {
  label: string;
  icon: GtkStock.id option;
  sections: pref_section list;
  children: pref_category list;
}

type pref_widget = { widget: GObj.widget; apply: unit -> unit }

type return_button = Preferences_Apply | Preferences_OK | Preferences_Cancel

let pref_category label ?icon ?(children = []) sections =
  { label; icon; sections; children }

let create_preferences_box pref_struct current_section =
  let main_box = GPack.hbox () in

  let columns = new GTree.column_list in
  let icon_col = columns#add GtkStock.conv
  and label_col = columns#add Gobject.Data.string
  and box_col = columns#add Gobject.Data.caml in
  columns#lock ();

  let pane = GPack.paned `HORIZONTAL ~packing:main_box#add () in

  (* Tree view part *)
  let scroll = GBin.scrolled_window ~hpolicy:`NEVER ~packing:pane#pack1 ()
  and tree = GTree.tree_store columns in
  let view = GTree.view ~model:tree ~headers_visible:false ~packing:scroll#add_with_viewport () in
  let selection = view#selection in
  selection#set_mode `SINGLE;

  let menu_box = GPack.vbox ~packing:pane#pack2 () in

  let renderer = (GTree.cell_renderer_pixbuf [], ["stock-id", icon_col]) in
  let col = GTree.view_column ~renderer () in
  let _ = view#append_column col in

  let renderer = (GTree.cell_renderer_text [], ["text", label_col]) in
  let col = GTree.view_column ~renderer () in
  let _ = view#append_column col in

  let make_section (main_box : #GPack.box) { label; box; apply } =
    let wf = GBin.frame ~label () in
    box#set_border_width 4;
    wf#add box#coerce;
    let widget = wf#coerce in
    main_box#pack ~expand:true ~padding:2 widget;
    { widget; apply }
  in

  (* Populate the tree *)

  let rec make_tree ?parent { label; icon; sections; children } =
    (* box is not shown at first *)
    let box = GPack.vbox ~packing:(menu_box#pack ~expand:true) ~show:false () in
    box#set_margin_left 4;
    let section_widgets = List.map (make_section box) sections in
    let box_widget = {
      widget = box#coerce;
      apply = fun () -> List.iter (fun { apply } -> apply ()) section_widgets;
    } in
    let new_iter = tree#append ?parent () in
    Option.iter (tree#set ~row:new_iter ~column:icon_col) icon;
    tree#set ~row:new_iter ~column:label_col label;
    tree#set ~row:new_iter ~column:box_col box_widget;
    List.iter (make_tree ~parent:new_iter) children;
  in

  List.iter make_tree pref_struct;

  (* Dealing with signals *)
  let current_widget : GObj.widget option ref = ref None in

  let select_path path =
    Option.iter (fun widget -> widget#misc#hide ()) !current_widget;
    current_section := Some (GTree.Path.get_indices path);
    let { widget } = tree#get ~row:(tree#get_iter path) ~column:box_col in
    widget#misc#show ();
    current_widget := Some widget;
  in

  let when_selected () =
    match selection#get_selected_rows with
    | row :: _ -> select_path row
    | _ -> ()
  in

  (* Focus on a box when selected *)
  let _ = selection#connect#changed ~callback:when_selected in

  view#expand_all ();
  Option.iter (fun path ->
      selection#select_path (GTree.Path.create (Array.to_list path));
    ) !current_section;

  {
    widget = (main_box :> GObj.widget);
    apply = fun () ->
      let foreach _ iter =
        let { apply } = tree#get ~row:iter ~column:box_col in
        apply ();
        false
      in
      tree#foreach foreach;
  }

let edit_preferences
    ?(apply = fun () -> ())
    title ?parent ?width ?height ?(current_section = ref None)
    pref_struct =
  let dialog = GWindow.dialog
    ~position:`CENTER
    ~modal:true ~title
    ~type_hint:`DIALOG
    ?parent ?height ?width
    ()
  in
  let preferences_box = create_preferences_box pref_struct current_section in

  dialog#vbox#pack ~expand:true preferences_box.widget#coerce;

  dialog#add_button_stock `APPLY `APPLY;
  dialog#add_button_stock `OK `OK;
  dialog#add_button_stock `CANCEL `CANCEL;

  let rec iter rep =
    try
      match dialog#run () with
        | `APPLY  -> preferences_box.apply (); iter Preferences_Apply
        | `OK -> preferences_box.apply (); dialog#destroy (); Preferences_OK
        | _ -> dialog#destroy (); rep
    with exc ->
      GToolbox.message_box ~title:"Error"
          (match exc with Failure str -> str | _ -> Printexc.to_string exc);
      iter rep
  in
  iter Preferences_Cancel

(** Generic types and functions for preference controls *)

type pref_ui_specific_info =
| PBool of { pref: bool preference }
| PInt of { min: int; max: int; pref: int preference }
| PTextExpr of { initial: string; set: (string -> unit) }
| PCombo of { values: string list; editable: bool; initial: string; set: (int -> string -> unit) }
| PModifiers of { allowed_get: unit -> Gdk.Tags.modifier list;
    registerer: ((unit -> unit) -> unit) option; callback: (unit -> unit) option; pref: string preference }

type pref_ui_info = {
  text: string;
  tooltip: string option;
  specific: pref_ui_specific_info;
}

let string_of_modifier = function
  | `CONTROL -> "<ctrl>"
  | `SHIFT -> "<shift>"
  | `META -> if Coq_config.arch = "Darwin" then "<cmd>" else "<meta>"
  | `MOD1 -> "<alt>"
  | `MOD2 -> "<mod2>"
  | `MOD3 -> "<mod3>"
  | `MOD4 -> "<mod4>"
  | `MOD5 -> "<mod5>"
  | _  -> ""

let all_modifiers = [`CONTROL; `SHIFT; `META; `MOD1; `MOD2; `MOD3; `MOD4; `MOD5]

let pbool text ?tooltip pref = { text; tooltip; specific = PBool { pref } }
let pint text ?tooltip ?(min = 0) ?(max = 999_999_999) pref =
  { text; tooltip; specific = PInt { min; max; pref } }
let ptextexpr text ?tooltip initial set = { text; tooltip; specific = PTextExpr { initial; set } }
let pstring text ?tooltip pref = ptextexpr text ?tooltip pref#get pref#set
let pcombo text ?tooltip values ?(editable = false) initial set =
  { text; tooltip; specific = PCombo { values; editable; initial; set } }
let pstringcombo text ?tooltip values ?(editable = false) pref =
  pcombo text ?tooltip values ~editable pref#get (fun _ s -> pref#set s)
let pmodifiers text ?tooltip allowed_get ?registerer ?callback pref =
  { text; tooltip; specific = PModifiers { allowed_get; registerer; callback; pref } }

let create_pref_section ?(in_grid = false) label pref_data =
  let box = GPack.vbox () in
  let add_row =
    let create_label pref_data packing =
      if pref_data.text <> "" then
        let label = GMisc.label ~text:pref_data.text ~xalign:0. ~packing () in
        Option.iter label#set_tooltip_text pref_data.tooltip;
    in
    if in_grid then
      let grid = GPack.grid ~col_spacings:4 ~packing:box#pack () in
      (fun pref_data index ->
        create_label pref_data (grid#attach ~left:0 ~top:index);
        ((grid#attach ~left:1 ~top:index) : (GObj.widget -> unit)))
    else
      (fun pref_data _ ->
        let box = GPack.hbox ~packing:box#pack () in
        create_label pref_data box#pack;
        (box#pack : (GObj.widget -> unit)))
  in

  let add_param index pref_data =
    let setup_input ?(expand = false) input =
      if not in_grid && pref_data.text <> "" then
        input#set_margin_left 4;
      Option.iter input#set_tooltip_text pref_data.tooltip;
      input#set_hexpand expand;
    in
    match pref_data.specific with
    | PBool { pref } ->
        let input = GButton.check_button ~label:pref_data.text ~active:pref#get ~packing:box#pack () in
        ignore (input#connect#toggled ~callback:(fun () -> pref#set input#active));
    | PInt { min; max; pref } ->
        let packing = add_row pref_data index in
        let input = GEdit.spin_button
          ~numeric:true ~update_policy:`IF_VALID ~digits:0
          ~packing ()
        in
        setup_input input;
        let lower = float_of_int min and upper = float_of_int max in
        input#adjustment#set_bounds ~lower ~upper ~step_incr:1. ();
        input#set_value (float_of_int pref#get);
        ignore (input#connect#value_changed ~callback:(fun () -> pref#set input#value_as_int));
    | PTextExpr { initial; set } ->
        let packing = add_row pref_data index in
        let input = GEdit.entry ~text:initial ~packing () in
        setup_input ~expand:true input;
        ignore (input#connect#changed ~callback:(fun () -> set input#text));
    | PCombo { values; editable; initial; set } when editable ->
        let packing = add_row pref_data index in
        let input, _ = GEdit.combo_box_entry_text ~strings:values ~packing () in
        input#entry#set_editable true;
        input#entry#set_text initial;
        setup_input ~expand:true input;
        ignore (input#connect#changed ~callback:(fun () -> set input#active input#entry#text));
    | PCombo { values; editable; initial; set } ->
        let packing = add_row pref_data index in
        let active =
          (let rec find_index i = function
           | [] -> None
           | h :: _ when h = initial -> Some i
           | _ :: t -> find_index (succ i) t
           in
           find_index 0 values)
        in
        let input, _ = GEdit.combo_box_text ~strings:values ?active ~packing () in
        setup_input input;
        let values = Array.of_list values in
        let callback () = set input#active values.(input#active) in
        ignore (input#connect#changed ~callback);
    | PModifiers { allowed_get; registerer; callback; pref } ->
        let packing = add_row pref_data index in
        let hbox = GPack.hbox ~packing () in
        setup_input hbox;
        let allowed_mods = allowed_get () in
        let filter_mods allowed = List.filter (fun modifier -> List.mem modifier allowed) in
        let value = ref (filter_mods allowed_mods (str_to_mod_list pref#get)) in
        let create_button modifier =
          let but = GButton.toggle_button
            ~label:(string_of_modifier modifier)
            ~active:(List.mem modifier !value)
            ~packing:hbox#pack () in
          ignore (but#connect#toggled
            ~callback:(fun _ ->
              if but#active then value := modifier::!value
              else value := List.filter ((<>) modifier) !value;
              pref#set (mod_list_to_str !value);
              Option.iter (fun callback -> callback ()) callback;
            )
          );
        in
        List.iter create_button allowed_mods;
        Option.iter (fun registerer ->
            registerer (fun () ->
              List.iter hbox#remove hbox#children;
              let allowed_mods = allowed_get () in
              value := filter_mods allowed_mods !value;
              List.iter create_button allowed_mods;
            )
          ) registerer;
  in
  List.iteri add_param pref_data;
  { label; box; apply = fun () -> () }

class tag_button (box : Gtk.box Gtk.obj) =
object (self)

  inherit GObj.widget box

  val fg_color = GButton.color_button ()
  val fg_unset = GButton.toggle_button ()
  val bg_color = GButton.color_button ()
  val bg_unset = GButton.toggle_button ()
  val bold = GButton.toggle_button ()
  val italic = GButton.toggle_button ()
  val underline = GButton.toggle_button ()
  val strikethrough = GButton.toggle_button ()

  method set_tag tag =
    let track c but set = match c with
    | None -> set#set_active true
    | Some c ->
      set#set_active false;
      but#set_color (Gdk.Color.color_parse c)
    in
    track tag.tag_bg_color bg_color bg_unset;
    track tag.tag_fg_color fg_color fg_unset;
    bold#set_active tag.tag_bold;
    italic#set_active tag.tag_italic;
    underline#set_active tag.tag_underline;
    strikethrough#set_active tag.tag_strikethrough;

  method tag =
    let get but set =
      if set#active then None
      else Some (Gdk.Color.color_to_string but#color)
    in
    {
      tag_bg_color = get bg_color bg_unset;
      tag_fg_color = get fg_color fg_unset;
      tag_bold = bold#active;
      tag_italic = italic#active;
      tag_underline = underline#active;
      tag_strikethrough = strikethrough#active;
    }

  initializer
    let box = new GPack.box box in
    let set_stock button stock =
      let stock = GMisc.image ~stock ~icon_size:`BUTTON () in
      button#set_image stock#coerce
    in
    let bprops button ?stock tooltip =
      (button :> GButton.button_skel), stock, tooltip
    in
    List.iter (fun (button, stock, tooltip) ->
        Option.iter (set_stock button) stock;
        button#set_tooltip_text tooltip;
        box#pack button#coerce;
      ) [
        bprops fg_color "Foreground color";
        bprops fg_unset ~stock:`CANCEL "Disable foreground color";
        bprops bg_color "Background color";
        bprops bg_unset ~stock:`CANCEL "Disable background color";
        bprops bold ~stock:`BOLD "Bold";
        bprops italic ~stock:`ITALIC "Italic";
        bprops underline ~stock:`UNDERLINE "Underline";
        bprops strikethrough ~stock:`STRIKETHROUGH "Strikethrough";
      ];
    let cb but obj = obj#set_sensitive (not but#active) in
    let _ = fg_unset#connect#toggled ~callback:(fun () -> cb fg_unset fg_color#misc) in
    let _ = bg_unset#connect#toggled ~callback:(fun () -> cb bg_unset bg_color#misc) in
    ()

end

let tag_button () =
  let box = GPack.hbox () in
  new tag_button (Gobject.unsafe_cast box#as_widget)

let current_section = ref None

let configure ?(apply = fun () -> ()) parent =
  let config_font =
    let preview_text = "Goal (∃n : nat, n ≤ 0)∧(∀x,y,z, x∈y⋃z↔x∈y∨x∈z)." in
    match Coq_config.arch with
    | "Darwin" ->
      (* A poor man's font selection. Work around #16136, which is ultimately a GTK bug. *)
      let box = GPack.vbox () in
      box#set_height_request 200;
      box#set_height_request 300;
      let font_sel = GEdit.entry ~text:text_font#get () in
      box#pack ~expand:false font_sel#coerce;
      let text = GMisc.label ~text:preview_text () in
      text#set_ypad 10;
      box#pack ~expand:false text#coerce;
      let set_font () =
        let fd = GPango.font_description_from_string font_sel#text in
        text_font#set fd#to_string;
        text#misc#modify_font_by_name fd#to_string;
        font_sel#set_text fd#to_string;
      in
      text#misc#modify_font_by_name text_font#get;
      let _ = font_sel#connect#activate ~callback:set_font in
      { label = "Text font"; box; apply = set_font }
    | _ ->
      let box = GPack.hbox () in
      let font_sel = GMisc.font_selection () in
      font_sel#set_preview_text preview_text;
      box#pack ~expand:true font_sel#coerce;
      let _ = font_sel#misc#connect#realize
                ~callback:(fun () -> font_sel#set_font_name text_font#get) in
      { label = "Text font"; box; apply = (fun () -> text_font#set font_sel#font_name) }
  in

  let config_highlight =
    let source_param text ids elem_get pref =
      let id_to_name id =
        match elem_get id with
        | Some elem -> elem#name
        | None -> ""
      in
      let names = List.map id_to_name ids in
      let ids = Array.of_list ids in
      pcombo text names (id_to_name pref#get) (fun i _ -> pref#set ids.(i))
    in
    create_pref_section ~in_grid:true "Highlight configuration" [
        source_param "Style scheme:" style_manager#style_scheme_ids
          style_manager#style_scheme source_style;
        source_param "Language:"
          (List.filter (String.starts_with ~prefix:"coq") lang_manager#language_ids)
          lang_manager#language source_language;
      ]
  in

  let config_color =
    let box = GPack.vbox () in
    let grid = GPack.grid
      ~row_spacings:5
      ~col_spacings:5
      ~border_width:4
      ~packing:(box#pack ~expand:true) ()
    in
    let reset_button = GButton.button
      ~label:"Reset"
      ~packing:box#pack ()
    in
    let _ = GMisc.label ~text:"Background" ~packing:(grid#attach ~left:1 ~top: 0) () in
    let _ = GMisc.label ~text:"Foreground" ~packing:(grid#attach ~left:2 ~top: 0) () in
    let iter i (text, prefs) =
      let top = i + 1 in
      let _ = GMisc.label
        ~text ~xalign:0. ~packing:(grid#attach (*~expand:`X*) ~left:0 ~top) ()
      in
      List.iteri (fun i pref ->
        let button = GButton.color_button
          ~color:(Gdk.Color.color_parse pref#get)
          ~packing:(grid#attach ~left:(i + 1) ~top) ()
        in
        let _ = button#connect#color_set ~callback:begin fun () ->
          pref#set (Gdk.Color.color_to_string button#color)
        end in
        let reset _ =
          pref#reset ();
          button#set_color (Gdk.Color.color_parse pref#get)
        in
        let _ = reset_button#connect#clicked ~callback:reset in
        ()) prefs;
    in
    let () = Util.List.iteri iter [
      (*"Editor", [background_color]; *)
      "Processed text", [processed_color];
      "Text being processed", [processing_color];
      "Incompletely processed Qed", [incompletely_processed_color];
      "Breakpoints", [breakpoint_color];
      "Debugger stopping point", [db_stopping_point_color];
      "Errors", [error_color; error_fg_color];
    ] in
    let label = "Color configuration" in
    { label; box; apply = (fun () -> ()) }
  in

  let config_tags =
    let box = GPack.vbox () in
    let scroll = GBin.scrolled_window
      ~hpolicy:`NEVER
      ~vpolicy:`AUTOMATIC
      ~packing:(box#pack ~expand:true)
      ()
    in
    let grid = GPack.grid
      ~row_spacings:5
      ~col_spacings:5
      ~border_width:2
      ~packing:scroll#add_with_viewport ()
    in
    let i = ref 0 in
    let cb = ref [] in
    let iter text tag =
      let _ = GMisc.label
        ~text ~xalign:0. ~packing:(grid#attach (*~expand:`X*) ~left:0 ~top:!i) ()
      in
      let button = tag_button () in
      let callback () = tag#set button#tag in
      button#set_tag tag#get;
      grid#attach ~left:1 ~top:!i button#coerce;
      incr i;
      cb := callback :: !cb;
    in
    Util.String.Map.iter iter (list_tags ());
    let label = "Tag configuration" in
    let callback () = List.iter (fun f -> f ()) !cb in
    { label; box; apply = callback }
  in

  let editor_appearance =
    create_pref_section "Editor appearance" [
        pbool "Dynamic word wrap" dynamic_word_wrap;
        pbool "Show line number" show_line_number;
        pbool "Show spaces" show_spaces;
        pbool "Show right margin" show_right_margin;
        pbool "Show progress bar" show_progress_bar;
        pbool "Highlight current line" highlight_current_line;
      ]
  in
  let editor_behavior =
    create_pref_section "Editor behavior" [
        pbool "Auto indentation" auto_indent;
        pbool "Insert spaces instead of tabs" spaces_instead_of_tabs;
        pbool "Unicode binding completion" unicode_binding;
        pbool "Auto completion" auto_complete;
        pint "Delay (ms):" ~max:5000 auto_complete_delay;
        pbool "Emacs/PG keybindings (μPG mode)" microPG;
      ]
  in

  let global_auto_revert =
    create_pref_section "Global auto revert" [
      pbool "Enable" global_auto_revert;
      pint "Delay (ms):" global_auto_revert_delay;
    ]
  in

  let auto_save =
    create_pref_section "Auto save" [
      pbool "Enable" auto_save;
      pint "Delay (ms):" auto_save_delay;
      (* auto_save_name *)
    ]
  in

  let file_format =
    create_pref_section ~in_grid:true "File format" [
      pcombo "File charset encoding:" ~editable:true
        ("UTF-8" :: "LOCALE" :: match encoding#get with Emanual s -> [s] | _ -> [])
        (string_of_inputenc encoding#get)
        (fun _ s -> encoding#set (inputenc_of_string s));
      pcombo "EOL character:" ["Default"; {|Linux (\n)|}; {|Windows (\r\n)|}]
        (line_end_to_string line_ending#get)
        (fun _ s -> line_ending#set (line_end_of_string s));
    ]
  in

  let project_management =
    create_pref_section "Project management" [
        pstring "Default name for project file:" project_file_name;
        pcombo "Project file options are" (List.map string_of_project_behavior [Subst_args; Append_args; Ignore_args])
          (string_of_project_behavior read_project#get)
          (fun _ s -> read_project#set (project_behavior_of_string s));
      ];
  in

  let config_window =
    create_pref_section ~in_grid:true "Window" [
      pint "Width at startup:" window_width;
      pint "Height at startup:" window_height;
    ]
  in

  let cmd_editor =
    let predefined = ["emacs %s"; "vi %s"; "NOTEPAD %s"] in
    pstringcombo ~tooltip:"(%s for file name)" "External editor:" ~editable:true
      (predefined @ [if List.mem cmd_editor#get predefined then "" else cmd_editor#get])
      cmd_editor
  in
  let cmd_browse =
    let predefined = [
      Coq_config.browser;
      "netscape -remote \"openURL(%s)\"";
      "mozilla -remote \"openURL(%s)\"";
      "firefox -remote \"openURL(%s,new-windows)\" || firefox %s &";
      "seamonkey -remote \"openURL(%s)\" || seamonkey %s &"
    ] in
    pstringcombo ~tooltip:"(%s for url)" "Browser:" ~editable:true
      (predefined @ [if List.mem cmd_browse#get predefined then "" else cmd_browse#get])
      cmd_browse
  in
  let externals_cmds =
    create_pref_section ~in_grid:true "Commands for external programs" [
        ptextexpr "coqidetop:"
          (match cmd_coqtop#get with | None -> "AUTO" | Some x -> x)
          (fun s -> cmd_coqtop#set (if s = "AUTO" then None else Some s));
        pstring "coqc:" cmd_coqc;
        pstring "make:" cmd_make;
        pstring "coqmakefile:" cmd_coqmakefile;
        pstring "coqdoc:" cmd_coqdoc;
        pstring "Print ps:" cmd_print;
        cmd_editor;
        cmd_browse;
      ]
  in

  let modifiers_valid_callbacks = ref [] in
  let modifiers_reg callback =
    modifiers_valid_callbacks := List.cons callback !modifiers_valid_callbacks
  in
  let allowed_mods_get () = str_to_mod_list modifiers_valid#get in
  let modifiers_valid =
    create_pref_section "Allowed modifiers" [
        pmodifiers "" (fun () -> all_modifiers) ~callback:(fun () ->
            List.iter (fun callback -> callback ()) !modifiers_valid_callbacks
          ) modifiers_valid
    ]
  in
  let config_modifiers =
    create_pref_section ~in_grid:true "Modifiers for menu items accelerators" [
        pmodifiers "View:" allowed_mods_get ~registerer:modifiers_reg modifier_for_display;
        pmodifiers "Navigation:" allowed_mods_get ~registerer:modifiers_reg modifier_for_navigation;
        pmodifiers "Templates:" allowed_mods_get ~registerer:modifiers_reg modifier_for_templates;
        pmodifiers "Queries:" allowed_mods_get ~registerer:modifiers_reg modifier_for_queries;
        (* user_queries *)
      ]
  in

  let misc =
    create_pref_section "Miscellaneous (unimplemented?)" [
      pbool "Stop interpreting before the current point" stop_before;
      pbool "Reset coqtop on tab switch" reset_on_tab_switch;
      pbool "Vertical tabs" vertical_tabs;
      pbool "Tabs on opposite side" opposite_tabs;
    ]
  in

(*
  let add_user_query () =
    let input_string l v =
      match GToolbox.input_string ~title:l v with
      | None -> ""
      | Some s -> s
    in
    let q = input_string "User query" "Your query" in
    let k = input_string "Shortcut key" "Shortcut (a single letter)" in
    let q = CString.map (fun c -> if c = '$' then ' ' else c) q in
    (* Anything that is not a simple letter will be ignored. *)
    let k =
      if Int.equal (CString.length k) 1 && Util.is_letter k.[0] then k
      else "" in
    let k = String.uppercase_ascii k in
      [q, k]
  in

  let user_queries =
    list
      ~f:user_queries#set
      (* Disallow same query, key or empty query. *)
      ~eq:(fun (q1, k1) (q2, k2) -> k1 = k2 || q1 = "" || q2 = "" || q1 = q2)
      ~add:add_user_query
      ~titles:["User query"; "Shortcut key"]
      "User queries"
      (fun (q, s) -> [q; s])
      user_queries#get

  in
*)

  let cmds = [
      pref_category "Files" ~icon:`FLOPPY [file_format; global_auto_revert; auto_save] ~children:[
          pref_category "Project" ~icon:`PAGE_SETUP [project_management];
        ];
      pref_category "Editor" ~icon:`EDIT [editor_appearance; editor_behavior] ~children:[
          pref_category "Font" ~icon:`SELECT_FONT [config_font];
          pref_category "Colors" ~icon:`SELECT_COLOR [config_highlight; config_color];
        ];
      pref_category "Appearance" ~icon:`ZOOM_FIT [config_window] ~children:[
          pref_category "Tags" ~icon:`SELECT_COLOR [config_tags];
        ];
      pref_category "Externals" ~icon:`EXECUTE [externals_cmds];
      pref_category "Shortcuts" ~icon:`MEDIA_FORWARD [modifiers_valid; config_modifiers];
      pref_category "Misc" ~icon:`PREFERENCES [misc];
    ]
  in

  if (edit_preferences ~apply "Preferences" ~parent ~current_section ~width:600 ~height:400 cmds) <> Preferences_Cancel then
    save_pref ();

(** Copying lablgtk question_box + forbidding hiding *)

type button_contents =
  | ButtonUseStock of GtkStock.id
  | ButtonUseString of string

let question_box ?parent ?icon ~title ?(buttons = []) ?(default = 1) message =
  let button_nb = ref 0 in
  let window = GWindow.dialog ~position:`CENTER ~modal:true ?parent ~type_hint:`DIALOG ~title () in
  let hbox = GPack.hbox ~border_width:10 ~packing:window#vbox#add () in
  let bbox = window#action_area in
  Option.iter (fun icon -> hbox#pack icon#coerce ~padding:4) icon;
  ignore (GMisc.label ~text: message ~packing: hbox#add ());
  (* the function called to create each button by iterating *)
  let rec iter_buttons pos buttons =
    if buttons <> [] then (
      let label, stock =
        match List.hd buttons with
        | ButtonUseString label -> Some label, None
        | ButtonUseStock stock -> None, Some stock
      in
      let but =
        GButton.button ?label ?stock ~packing:(bbox#pack ~expand:true ~padding:4) ()
      in
      let _ = but#connect#clicked ~callback:(fun () -> button_nb := pos; window#destroy ()) in
      (* If it's the default button then give it the focus *)
      if pos = default then
        but#grab_default ();
      iter_buttons (pos + 1) (List.tl buttons);
    );
  in
  iter_buttons 1 buttons;
  let _ = window#connect#destroy ~callback: GMain.Main.quit in
  window#set_position `CENTER;
  window#show ();
  GMain.Main.main ();
  !button_nb

let message_box ?parent ?icon ~title ?(ok = ButtonUseStock `OK) message =
  ignore (question_box ?parent ?icon ~title ~buttons:[ ok ] message)
