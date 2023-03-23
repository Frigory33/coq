(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Configwin

let lang_manager = GSourceView3.source_language_manager ~default:true
let () = lang_manager#set_search_path
  ((Minilib.coqide_data_dirs ())@lang_manager#search_path)
let style_manager = GSourceView3.source_style_scheme_manager ~default:true
let () = style_manager#set_search_path
  ((Minilib.coqide_data_dirs ())@style_manager#search_path)

type tag = {
  tag_fg_color : string option;
  tag_bg_color : string option;
  tag_bold : bool;
  tag_italic : bool;
  tag_underline : bool;
  tag_strikethrough : bool;
}


(** Generic preferences *)

type obj = {
  set : string list -> unit;
  get : unit -> string list;
}

let preferences : obj Util.String.Map.t ref = ref Util.String.Map.empty
let unknown_preferences : string list Util.String.Map.t ref = ref Util.String.Map.empty

class type ['a] repr =
object
  method into : string list -> 'a option
  method from : 'a -> string list
end

class ['a] preference_signals ~(changed : 'a GUtil.signal) =
object
  inherit GUtil.ml_signals [changed#disconnect]
  method changed = changed#connect ~after
end

class ['a] preference ~(name : string list) ~(init : 'a) ~(repr : 'a repr) =
object (self)
  initializer
    let set v = match repr#into v with None -> () | Some s -> self#set s in
    let get () = repr#from self#get in
    let obj = { set = set; get = get; } in
    let name = String.concat "." name in
    if Util.String.Map.mem name !preferences then
      invalid_arg ("Preference " ^ name ^ " already exists")
    else
      preferences := Util.String.Map.add name obj !preferences

  val default = init
  val mutable data = init
  val changed : 'a GUtil.signal = new GUtil.signal ()
  val name : string list = name
  method connect = new preference_signals ~changed
  method get = data
  method set (n : 'a) = data <- n; changed#call n
  method reset () = self#set default
  method default = default
end

let stick (pref : 'a preference) (obj : < connect : #GObj.widget_signals ; .. >)
  (cb : 'a -> unit) =
  let _ = cb pref#get in
  let p_id = pref#connect#changed ~callback:(fun v -> cb v) in
  let _ = obj#connect#destroy ~callback:(fun () -> pref#connect#disconnect p_id) in
  ()

(** Useful marshallers *)

let mod_to_str m =
  match m with
  | `MOD1 -> "<Alt>"
  | `MOD2 -> "<Mod2>"
  | `MOD3 -> "<Mod3>"
  | `MOD4 -> "<Mod4>"
  | `MOD5 -> "<Mod5>"
  | `CONTROL -> "<Control>"
  | `SHIFT -> "<Shift>"
  | `HYPER -> "<Hyper>"
  | `META -> "<Meta>"
  | `RELEASE -> ""
  | `SUPER -> "<Super>"
  | `BUTTON1 | `BUTTON2 | `BUTTON3 | `BUTTON4 | `BUTTON5 | `LOCK -> ""

let mod_list_to_str l = List.fold_left (fun s m -> (mod_to_str m)^s) "" l

let str_to_mod_list s = snd (GtkData.AccelGroup.parse s)

type project_behavior = Ignore_args | Append_args | Subst_args

let string_of_project_behavior = function
| Ignore_args -> "ignored"
| Append_args -> "appended to arguments"
| Subst_args -> "taken instead of arguments"

let project_behavior_of_string = function
| "taken instead of arguments" -> Subst_args
| "appended to arguments" -> Append_args
| _ -> Ignore_args

type inputenc = Elocale | Eutf8 | Emanual of string

let string_of_inputenc = function
| Elocale -> "LOCALE"
| Eutf8 -> "UTF-8"
| Emanual s -> s

let inputenc_of_string s =
      (if s = "UTF-8" then Eutf8
       else if s = "LOCALE" then Elocale
       else Emanual s)

type line_ending = [ `DEFAULT | `WINDOWS | `UNIX ]

let line_end_of_string = function
| "Linux" -> `UNIX
| "Windows" -> `WINDOWS
| _ -> `DEFAULT

let line_end_to_string = function
| `UNIX -> "Linux"
| `WINDOWS -> "Windows"
| `DEFAULT -> "Default"

let use_default_doc_url = "(automatic)"

module Repr =
struct

let string : string repr =
object
  method from s = [s]
  method into = function [s] -> Some s | _ -> None
end

let string_pair : (string * string) repr =
object
  method from (s1, s2) = [s1; s2]
  method into = function [s1; s2] -> Some (s1, s2) | _ -> None
end

let string_list : string list repr =
object
  method from s = s
  method into s = Some s
end

let string_pair_list (sep : char) : (string * string) list repr =
object
  val sep' = String.make 1 sep
  method from = CList.map (fun (s1, s2) -> CString.concat sep' [s1; s2])
  method into l =
    try
      Some (CList.map (fun s ->
        let split = String.split_on_char sep s in
        CList.nth split 0, CList.nth split 1) l)
    with Failure _ -> None
end

let bool : bool repr =
object
  method from s = [string_of_bool s]
  method into = function
  | ["true"] -> Some true
  | ["false"] -> Some false
  | _ -> None
end

let int : int repr =
object
  method from s = [string_of_int s]
  method into = function
  | [i] -> (try Some (int_of_string i) with _ -> None)
  | _ -> None
end

let option (r : 'a repr) : 'a option repr =
object
  method from = function None -> [] | Some v -> "" :: r#from v
  method into = function
  | [] -> Some None
  | "" :: s -> Some (r#into s)
  | _ -> None
end

let custom (from : 'a -> string) (into : string -> 'a) : 'a repr =
object
  method from x = try [from x] with _ -> []
  method into = function
  | [s] -> (try Some (into s) with _ -> None)
  | _ -> None
end

let tag : tag repr =
let _to s = if s = "" then None else Some s in
let _of = function None -> "" | Some s -> s in
object
  method from tag = [
    _of tag.tag_fg_color;
    _of tag.tag_bg_color;
    string_of_bool tag.tag_bold;
    string_of_bool tag.tag_italic;
    string_of_bool tag.tag_underline;
    string_of_bool tag.tag_strikethrough;
  ]
  method into = function
  | [fg; bg; bd; it; ul; st] ->
    (try Some {
      tag_fg_color = _to fg;
      tag_bg_color = _to bg;
      tag_bold = bool_of_string bd;
      tag_italic = bool_of_string it;
      tag_underline = bool_of_string ul;
      tag_strikethrough = bool_of_string st;
      }
    with _ -> None)
  | _ -> None
end

end

let get_config_file dirs name =
  let find_config dir = Sys.file_exists (Filename.concat dir name) in
  let config_dir = List.find find_config dirs in
  Filename.concat config_dir name

let get_unicode_bindings_local_file () =
  try Some (get_config_file [Minilib.coqide_config_home ()] "coqide.bindings")
  with Not_found -> None

let get_unicode_bindings_default_file () =
  let name = "default.bindings" in
  let chk d = Sys.file_exists (Filename.concat d name) in
  try
    let dir = List.find chk (Minilib.coqide_data_dirs ()) in
    Some (Filename.concat dir name)
  with Not_found -> None

(** Hooks *)

let cmd_coqtop =
  new preference ~name:["cmd_coqtop"] ~init:None ~repr:Repr.(option string)

let cmd_coqc =
  new preference ~name:["cmd_coqc"] ~init:"coqc" ~repr:Repr.(string)

let cmd_make =
  new preference ~name:["cmd_make"] ~init:"make" ~repr:Repr.(string)

let cmd_coqmakefile =
  new preference ~name:["cmd_coqmakefile"] ~init:"coq_makefile -o makefile *.v" ~repr:Repr.(string)

let cmd_coqdoc =
  new preference ~name:["cmd_coqdoc"] ~init:"coqdoc -q -g" ~repr:Repr.(string)

let source_language =
  new preference ~name:["source_language"] ~init:"coq" ~repr:Repr.(string)

let source_style =
  new preference ~name:["source_style"] ~init:"coq_style" ~repr:Repr.(string)

let global_auto_revert =
  new preference ~name:["global_auto_revert"] ~init:false ~repr:Repr.(bool)

let global_auto_revert_delay =
  new preference ~name:["global_auto_revert_delay"] ~init:10000 ~repr:Repr.(int)

let auto_save =
  new preference ~name:["auto_save"] ~init:true ~repr:Repr.(bool)

let auto_save_delay =
  new preference ~name:["auto_save_delay"] ~init:10000 ~repr:Repr.(int)

let auto_save_name =
  new preference ~name:["auto_save_name"] ~init:("#","#") ~repr:Repr.(string_pair)

let read_project =
  let repr = Repr.custom string_of_project_behavior project_behavior_of_string in
  new preference ~name:["read_project"] ~init:Append_args ~repr

let project_file_name =
  new preference ~name:["project_file_name"] ~init:"_CoqProject" ~repr:Repr.(string)

let project_path =
  new preference ~name:["project_path"] ~init:None ~repr:Repr.(option string)

let encoding =
  let repr = Repr.custom string_of_inputenc inputenc_of_string in
  let init = if Sys.os_type = "Win32" then Eutf8 else Elocale in
  new preference ~name:["encoding"] ~init ~repr

let automatic_tactics =
  let init = ["trivial"; "tauto"; "auto"; "auto with *"; "intuition" ] in
  new preference ~name:["automatic_tactics"] ~init ~repr:Repr.(string_list)

let cmd_print =
  new preference ~name:["cmd_print"] ~init:"lpr" ~repr:Repr.(string)

let attach_modifiers (pref : string preference) prefix =
  let cb mds =
    let mds = str_to_mod_list mds in
    let change ~path ~key ~modi ~changed =
      if CString.is_sub prefix path 0 then
        ignore (GtkData.AccelMap.change_entry ~key ~modi:mds ~replace:true path)
    in
    GtkData.AccelMap.foreach change
  in
  pref#connect#changed ~callback:cb

let modifier_for_navigation =
  new preference ~name:["modifier_for_navigation"] ~init:"<Alt>" ~repr:Repr.(string)

let modifier_for_templates =
  new preference ~name:["modifier_for_templates"] ~init:"<Control><Shift>" ~repr:Repr.(string)

let select_arch m m_osx =
  if Coq_config.arch = "Darwin" then m_osx else m

let modifier_for_display =
  new preference ~name:["modifier_for_display"]
   (* Note: <Primary> (i.e. <Meta>, i.e. "Command") on Darwin, i.e. MacOS X, but <Alt> elsewhere *)
    ~init:(select_arch "<Alt><Shift>" "<Primary><Shift>")~repr:Repr.(string)

let modifier_for_queries =
  new preference ~name:["modifier_for_queries"] ~init:"<Control><Shift>" ~repr:Repr.(string)

let attach_modifiers_callback () =
  (* Tell to propagate changes done in preference menu to accel map *)
  (* To be done after the preferences are loaded *)
  let _ = attach_modifiers modifier_for_navigation "<Actions>/Navigation/" in
  let _ = attach_modifiers modifier_for_templates "<Actions>/Templates/" in
  let _ = attach_modifiers modifier_for_display "<Actions>/View/" in
  let _ = attach_modifiers modifier_for_queries "<Actions>/Queries/" in
  ()

let modifiers_valid =
  new preference ~name:["modifiers_valid"]
   (* Note: <Primary> is providing <Meta> (i.e. "Command") for Darwin, i.e. MacOS X *)
    ~init:"<Alt><Control><Shift><Primary>" ~repr:Repr.(string)

let browser_cmd_fmt =
 try
  let coq_netscape_remote_var = "COQREMOTEBROWSER" in
  Sys.getenv coq_netscape_remote_var
 with
  Not_found -> Coq_config.browser

let cmd_browse =
  new preference ~name:["cmd_browse"] ~init:browser_cmd_fmt ~repr:Repr.(string)

let cmd_editor =
  let init = if Sys.os_type = "Win32" then "NOTEPAD %s" else "emacs %s" in
  new preference ~name:["cmd_editor"] ~init ~repr:Repr.(string)

let text_font =
  let init = match Config.gtk_platform with
  | `QUARTZ -> "Arial Unicode MS 11"
  | _ -> "Monospace 10"
  in
  new preference ~name:["text_font"] ~init ~repr:Repr.(string)

let show_toolbar =
  new preference ~name:["show_toolbar"] ~init:true ~repr:Repr.(bool)

let window_width =
  new preference ~name:["window_width"] ~init:800 ~repr:Repr.(int)

let window_height =
  new preference ~name:["window_height"] ~init:600 ~repr:Repr.(int)

let unicode_binding =
  new preference ~name:["unicode_binding"] ~init:true ~repr:Repr.(bool)

let auto_complete =
  new preference ~name:["auto_complete"] ~init:false ~repr:Repr.(bool)

let auto_complete_delay =
  new preference ~name:["auto_complete_delay"] ~init:250 ~repr:Repr.(int)

let stop_before =
  new preference ~name:["stop_before"] ~init:true ~repr:Repr.(bool)

let reset_on_tab_switch =
  new preference ~name:["reset_on_tab_switch"] ~init:false ~repr:Repr.(bool)

let line_ending =
  let repr = Repr.custom line_end_to_string line_end_of_string in
  new preference ~name:["line_ending"] ~init:`DEFAULT ~repr

let vertical_tabs =
  new preference ~name:["vertical_tabs"] ~init:false ~repr:Repr.(bool)

let opposite_tabs =
  new preference ~name:["opposite_tabs"] ~init:false ~repr:Repr.(bool)

(* let background_color = *)
(*   new preference ~name:["background_color"] ~init:"cornsilk" ~repr:Repr.(string) *)

let attach_tag (pref : string preference) (tag : GText.tag) f =
  tag#set_property (f pref#get);
  pref#connect#changed ~callback:(fun c -> tag#set_property (f c))

let attach_bg (pref : string preference) (tag : GText.tag) =
  attach_tag pref tag (fun c -> `BACKGROUND c)

let attach_fg (pref : string preference) (tag : GText.tag) =
  attach_tag pref tag (fun c -> `FOREGROUND c)

let tags = ref Util.String.Map.empty

let list_tags () = !tags

let make_tag ?fg ?bg ?(bold = false) ?(italic = false) ?(underline = false) ?(strikethrough = false) () = {
  tag_fg_color = fg;
  tag_bg_color = bg;
  tag_bold = bold;
  tag_italic = italic;
  tag_underline = underline;
  tag_strikethrough = strikethrough;
}

let create_tag name default =
  let pref = new preference ~name:[name] ~init:default ~repr:Repr.(tag) in
  let set_tag tag =
    begin match pref#get.tag_bg_color with
    | None -> tag#set_property (`BACKGROUND_SET false)
    | Some c ->
      tag#set_property (`BACKGROUND_SET true);
      tag#set_property (`BACKGROUND c)
    end;
    begin match pref#get.tag_fg_color with
    | None -> tag#set_property (`FOREGROUND_SET false)
    | Some c ->
      tag#set_property (`FOREGROUND_SET true);
      tag#set_property (`FOREGROUND c)
    end;
    begin match pref#get.tag_bold with
    | false -> tag#set_property (`WEIGHT_SET false)
    | true ->
      tag#set_property (`WEIGHT_SET true);
      tag#set_property (`WEIGHT `BOLD)
    end;
    begin match pref#get.tag_italic with
    | false -> tag#set_property (`STYLE_SET false)
    | true ->
      tag#set_property (`STYLE_SET true);
      tag#set_property (`STYLE `ITALIC)
    end;
    begin match pref#get.tag_underline with
    | false -> tag#set_property (`UNDERLINE_SET false)
    | true ->
      tag#set_property (`UNDERLINE_SET true);
      tag#set_property (`UNDERLINE `SINGLE)
    end;
    begin match pref#get.tag_strikethrough with
    | false -> tag#set_property (`STRIKETHROUGH_SET false)
    | true ->
      tag#set_property (`STRIKETHROUGH_SET true);
      tag#set_property (`STRIKETHROUGH true)
    end;
  in
  let iter table =
    let tag = GText.tag ~name () in
    table#add tag#as_tag;
    ignore (pref#connect#changed ~callback:(fun _ -> set_tag tag));
    set_tag tag;
  in
  List.iter iter [Tags.Script.table; Tags.Proof.table; Tags.Message.table];
  tags := Util.String.Map.add name pref !tags

(* note these appear to only set the defaults; they don't override
the user selection from the Edit/Preferences/Tags dialog *)
let () =
  let iter (name, tag) = create_tag name tag in
  List.iter iter [
    ("constr.evar", make_tag ());
    ("constr.keyword", make_tag ~fg:"dark green" ());
    ("constr.notation", make_tag ());
    ("constr.path", make_tag ());
    ("constr.reference", make_tag ~fg:"navy"());
    ("constr.type", make_tag ~fg:"#008080" ());
    ("constr.variable", make_tag ());
    ("message.debug", make_tag ());
    ("message.error", make_tag ());
    ("message.warning", make_tag ());
    ("message.prompt", make_tag ~fg:"green" ());
    ("module.definition", make_tag ~fg:"orange red" ~bold:true ());
    ("module.keyword", make_tag ());
    ("tactic.keyword", make_tag ());
    ("tactic.primitive", make_tag ());
    ("tactic.string", make_tag ());
    ("diff.added", make_tag ~bg:"#b6f1c0" ~underline:true ());
    ("diff.removed", make_tag ~bg:"#f6b9c1" ~strikethrough:true ());
    ("diff.added.bg", make_tag ~bg:"#e9feee" ());
    ("diff.removed.bg", make_tag ~bg:"#fce9eb" ());
  ]

let processed_color =
  new preference ~name:["processed_color"] ~init:"light green" ~repr:Repr.(string)

let _ = attach_bg processed_color Tags.Script.processed
let _ = attach_bg processed_color Tags.Proof.highlight

let processing_color =
  new preference ~name:["processing_color"] ~init:"light blue" ~repr:Repr.(string)

let incompletely_processed_color =
  new preference ~name:["incompletely_processed_color"] ~init:"light sky blue" ~repr:Repr.(string)

let _ = attach_bg processing_color Tags.Script.to_process
let _ = attach_bg incompletely_processed_color Tags.Script.incomplete

let breakpoint_color =
  new preference ~name:["breakpoint_color"] ~init:"#db5860" ~repr:Repr.(string)
let white = (* worth showing on preferences menu?? *)
  new preference ~name:["white"] ~init:"white" ~repr:Repr.(string)

let _ = attach_bg breakpoint_color Tags.Script.breakpoint
let _ = attach_fg white Tags.Script.breakpoint

let db_stopping_point_color =
  new preference ~name:["db_stopping_point_color"] ~init:"#2154a6" ~repr:Repr.(string)

let _ = attach_bg db_stopping_point_color Tags.Script.debugging
let _ = attach_fg white Tags.Script.debugging

let error_color =
  new preference ~name:["error_color"] ~init:"#FFCCCC" ~repr:Repr.(string)

let _ = attach_bg error_color Tags.Script.error_bg

let error_fg_color =
  new preference ~name:["error_fg_color"] ~init:"red" ~repr:Repr.(string)

let _ = attach_fg error_fg_color Tags.Script.error

let dynamic_word_wrap =
  new preference ~name:["dynamic_word_wrap"] ~init:false ~repr:Repr.(bool)

let show_line_number =
  new preference ~name:["show_line_number"] ~init:false ~repr:Repr.(bool)

let auto_indent =
  new preference ~name:["auto_indent"] ~init:false ~repr:Repr.(bool)

let show_spaces =
  new preference ~name:["show_spaces"] ~init:true ~repr:Repr.(bool)

let show_right_margin =
  new preference ~name:["show_right_margin"] ~init:false ~repr:Repr.(bool)

let show_progress_bar =
  new preference ~name:["show_progress_bar"] ~init:true ~repr:Repr.(bool)

let spaces_instead_of_tabs =
  new preference ~name:["spaces_instead_of_tabs"] ~init:true ~repr:Repr.(bool)

let tab_length =
  new preference ~name:["tab_length"] ~init:2 ~repr:Repr.(int)

let highlight_current_line =
  new preference ~name:["highlight_current_line"] ~init:false ~repr:Repr.(bool)

let microPG =
  (* Legacy name in preference is "nanoPG" *)
  new preference ~name:["nanoPG"] ~init:false ~repr:Repr.(bool)

let user_queries =
  new preference ~name:["user_queries"] ~init:[] ~repr:Repr.(string_pair_list '$')

let diffs =
  new preference ~name:["diffs"] ~init:"off" ~repr:Repr.(string)

let current_section = ref None

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

(** Loading/saving preferences *)

let pref_file = Filename.concat (Minilib.coqide_config_home ()) "coqiderc"
let accel_file = Filename.concat (Minilib.coqide_config_home ()) "coqide.keys"

let accel_regex = Str.regexp {|\(; \|\)(gtk_accel_path "\([^""]*\)"|}

exception UnknownFormat

(* Attention: this only works with absolute normalized paths -
   which can be assumed here since the path comes from  Glib.get_user_config_dir *)
let rec mkdir_p path perms =
  if not (Sys.file_exists path)
  then
    let parent = Filename.dirname path in
    if not (Sys.file_exists parent) && parent<>path
    then mkdir_p parent perms
    else Unix.mkdir path perms
  else ()

let save_accel_pref () =
  mkdir_p (Minilib.coqide_config_home ()) 0o700;
  let tmp_file, fd = Filename.open_temp_file ?perms:(Some 0o644)
    ?temp_dir:(Some (Filename.dirname accel_file))
    "coqide.keys_" "" in
  close_out fd;
  GtkData.AccelMap.save tmp_file;
  (* AccelMap.save writes entries in random order, so sort them: *)
  let fd = open_in tmp_file in
  let map = ref CString.Map.empty in
  let top_lines = ref [] in
  begin
    try
      while true do
        let line = input_line fd in
        if Str.string_match accel_regex line 0 then
          let key = Str.matched_group 2 line in
          map := CString.Map.add key line !map
        else begin
          if not (CString.Map.is_empty !map) then begin
            Minilib.log ("Unknown format for coqide.keys; sorting skipped");
            raise UnknownFormat
          end;
          top_lines := line :: !top_lines
        end
      done
    with
    | UnknownFormat -> close_in fd
    | End_of_file ->
      close_in fd;
      let fd = open_out tmp_file in
      List.iter (fun l -> Printf.fprintf fd "%s\n" l) (List.rev !top_lines);
      CString.Map.iter (fun k l -> Printf.fprintf fd "%s\n" l) !map;
      close_out fd
  end;
  Sys.rename tmp_file accel_file

let save_rc_pref () =
  mkdir_p (Minilib.coqide_config_home ()) 0o700;
  let add = Util.String.Map.add in
  let fold key obj accu = add key (obj.get ()) accu in
  let prefs = Util.String.Map.fold fold !preferences Util.String.Map.empty in
  let prefs = Util.String.Map.fold Util.String.Map.add !unknown_preferences prefs in
  Config_lexer.print_file pref_file prefs

let save_pref () =
  save_accel_pref ();
  save_rc_pref ()

let try_load_pref_file loader warn file =
  try
    loader file
  with
    e -> warn ~delay:5000 ("Could not load " ^ file ^ " ("^Printexc.to_string e^")")

let load_pref_file loader warn name =
  try
    let user_file = get_config_file [Minilib.coqide_config_home ()] name in
    warn ~delay:2000 ("Loading " ^ user_file);
    try_load_pref_file loader warn user_file
  with Not_found ->
  try
    let system_wide_file = get_config_file (Minilib.coqide_system_config_dirs ()) name in
    warn ~delay:5000 ("No user " ^ name ^ ", using system wide configuration");
    try_load_pref_file loader warn system_wide_file
  with Not_found ->
  (* Compatibility with oldest versions of CoqIDE (<= 8.4) *)
  try
    let old_user_file = get_config_file [Option.default "" (Glib.get_home_dir ())] ("."^name) in
    warn ~delay:5000 ("No " ^ name ^ ", trying to recover from an older version of CoqIDE");
    try_load_pref_file loader warn old_user_file
  with Not_found ->
  (* Built-in configuration *)
    warn ~delay:5000 ("No " ^ name ^ ", using default internal configuration")

let load_accel_pref ~warn =
  load_pref_file GtkData.AccelMap.load warn "coqide.keys"

let load_rc_pref ~warn =
  let loader file =
    let m = Config_lexer.load_file file in
    let iter name v =
      if Util.String.Map.mem name !preferences then
        try (Util.String.Map.find name !preferences).set v with _ -> ()
      else unknown_preferences := Util.String.Map.add name v !unknown_preferences
    in
    Util.String.Map.iter iter m in
  load_pref_file loader warn "coqiderc";
  attach_modifiers_callback ()

let load_pref ~warn =
  load_rc_pref ~warn;
  load_accel_pref ~warn

type pref_specific_info =
| PBool of { pref: bool preference }
| PInt of { min: int; max: int; pref: int preference }
| PTextExpr of { initial: string; set: (string -> unit) }
| PCombo of { values: string list; editable: bool; initial: string; set: (int -> string -> unit) }
| PModifiers of { allowed_get: unit -> Gdk.Tags.modifier list;
    registerer: ((unit -> unit) -> unit) option; callback: (unit -> unit) option; pref: string preference }
type pref_info = {
  text: string;
  tooltip: string option;
  specific: pref_specific_info;
}

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

let create_pref_section ?(in_grid = false) ?label pref_data =
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
  let callback () = () in
  custom ?label box callback true

let configure ?(apply=(fun () -> ())) parent =
  let config_font = match Coq_config.arch with
  | "Darwin" ->
    (* A poor man's font selection. Work around #16136, which is ultimately a GTK bug. *)
    let box = GPack.vbox () in
    let () = box#set_height_request 200 in
    let () = box#set_height_request 300 in
    let w = GEdit.entry ~text:text_font#get () in
    let () = box#pack ~expand:false w#coerce in
    let text = GMisc.label ~text:"Goal (∃n : nat, n ≤ 0)∧(∀x,y,z, x∈y⋃z↔x∈y∨x∈z)." () in
    let () = text#set_ypad 10 in
    let () = box#pack ~expand:false text#coerce in
    let set_font () =
      let fd = GPango.font_description_from_string w#text in
      let () = text_font#set fd#to_string in
      let () = text#misc#modify_font_by_name fd#to_string in
      w#set_text fd#to_string
    in
    let () = text#misc#modify_font_by_name text_font#get in
    let _ = w#connect#activate ~callback:set_font in
    custom ~label:"Text font" box set_font true
  | _ ->
    let box = GPack.hbox () in
    let w = GMisc.font_selection () in
    w#set_preview_text
      "Goal (∃n : nat, n ≤ 0)∧(∀x,y,z, x∈y⋃z↔x∈y∨x∈z).";
    box#pack ~expand:true w#coerce;
    ignore (w#misc#connect#realize
              ~callback:(fun () -> w#set_font_name text_font#get));
    custom
      ~label:"Text font"
      box
      (fun () ->
         let fd =  w#font_name in
         text_font#set fd)
      true
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
    create_pref_section ~in_grid:true ~label:"Highlight configuration" [
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
(*       ("Editor", [background_color]); *)
      ("Processed text", [processed_color]);
      ("Text being processed", [processing_color]);
      ("Incompletely processed Qed", [incompletely_processed_color]);
      ("Breakpoints", [breakpoint_color]);
      ("Debugger stopping point", [db_stopping_point_color]);
      ("Errors", [error_color; error_fg_color]);
    ] in
    let label = "Color configuration" in
    let callback () = () in
    custom ~label box callback true
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
    let () = Util.String.Map.iter iter !tags in
    let label = "Tag configuration" in
    let callback () = List.iter (fun f -> f ()) !cb in
    custom ~label box callback true
  in

  let editor_appearance =
    create_pref_section ~label:"Editor appearance" [
        pbool "Dynamic word wrap" dynamic_word_wrap;
        pbool "Show line number" show_line_number;
        pbool "Show spaces" show_spaces;
        pbool "Show right margin" show_right_margin;
        pbool "Show progress bar" show_progress_bar;
        pbool "Highlight current line" highlight_current_line;
      ]
  in
  let editor_behavior =
    create_pref_section ~label:"Editor behavior" [
        pbool "Auto indentation" auto_indent;
        pbool "Insert spaces instead of tabs" spaces_instead_of_tabs;
        pbool "Unicode binding completion" unicode_binding;
        pbool "Auto completion" auto_complete;
        pint "Delay (ms):" ~max:5000 auto_complete_delay;
        pbool "Emacs/PG keybindings (μPG mode)" microPG;
      ]
  in

  let global_auto_revert =
    create_pref_section ~label:"Global auto revert" [
      pbool "Enable" global_auto_revert;
      pint "Delay (ms):" global_auto_revert_delay;
    ]
  in

  let auto_save =
    create_pref_section ~label:"Auto save" [
      pbool "Enable" auto_save;
      pint "Delay (ms):" auto_save_delay;
      (* auto_save_name *)
    ]
  in

  let file_format =
    create_pref_section ~in_grid:true ~label:"File format" [
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
    create_pref_section ~label:"Project management" [
        pstring "Default name for project file:" project_file_name;
        pcombo "Project file options are" (List.map string_of_project_behavior [Subst_args; Append_args; Ignore_args])
          (string_of_project_behavior read_project#get)
          (fun _ s -> read_project#set (project_behavior_of_string s));
      ];
  in

  let config_window =
    create_pref_section ~in_grid:true ~label:"Window" [
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
    create_pref_section ~in_grid:true ~label:"Commands for external programs" [
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
    create_pref_section ~label:"Allowed modifiers" [
        pmodifiers "" (fun () -> all_modifiers) ~callback:(fun () ->
            List.iter (fun callback -> callback ()) !modifiers_valid_callbacks
          ) modifiers_valid
    ]
  in
  let config_modifiers =
    create_pref_section ~in_grid:true ~label:"Modifiers for menu items accelerators" [
        pmodifiers "View:" allowed_mods_get ~registerer:modifiers_reg modifier_for_display;
        pmodifiers "Navigation:" allowed_mods_get ~registerer:modifiers_reg modifier_for_navigation;
        pmodifiers "Templates:" allowed_mods_get ~registerer:modifiers_reg modifier_for_templates;
        pmodifiers "Queries:" allowed_mods_get ~registerer:modifiers_reg modifier_for_queries;
        (* user_queries *)
      ]
  in

  let misc =
    create_pref_section ~label:"Miscellaneous" [
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
      Section("Files", Some `FLOPPY, [file_format; global_auto_revert; auto_save], [
          Section("Project", Some `PAGE_SETUP, [project_management], []);
        ]);
      Section("Editor", Some `EDIT, [editor_appearance; editor_behavior], [
          Section("Font", Some `SELECT_FONT, [config_font], []);
          Section("Colors", Some `SELECT_COLOR, [config_highlight; config_color], []);
        ]);
      Section("Appearance", Some `ZOOM_FIT, [config_window], [
          Section("Tags", Some `SELECT_COLOR, [config_tags], []);
        ]);
      Section("Externals", Some `EXECUTE, [externals_cmds], []);
      Section("Shortcuts", Some `MEDIA_FORWARD, [modifiers_valid; config_modifiers], []);
      Section("Misc", Some `PREFERENCES, [misc], [])
    ]
  in
(*
  Format.printf "before edit: current.text_font = %s@." (Pango.Font.to_string current.text_font);
*)
  let x = edit ~apply "Preferences" ~parent ~current_section ~width:600 ~height:400 cmds in
(*
  Format.printf "after edit: current.text_font = %s@." (Pango.Font.to_string current.text_font);
*)
  match x with
    | Return_apply | Return_ok -> save_pref ()
    | Return_cancel -> ()
