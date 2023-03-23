(*********************************************************************************)
(*                Cameleon                                                       *)
(*                                                                               *)
(*    Copyright (C) 2005 Institut National de Recherche en Informatique et       *)
(*    en Automatique. All rights reserved.                                       *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Library General Public License as            *)
(*    published by the Free Software Foundation; either version 2 of the         *)
(*    License, or  any later version.                                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Library General Public          *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** This module contains the gui functions of Configwin.*)

open Configwin_types

class type widget =
  object
    method box : GObj.widget
    method apply : unit -> unit
  end

let debug = false
let dbg s = if debug then Minilib.log s else ()

(** Class used to pack a custom box. *)
class custom_param_box param =
  let _ = dbg "custom_param_box" in
  let top =
    match param.custom_framed with
      None -> param.custom_box#coerce
    | Some l ->
        let wf = GBin.frame ~label: l () in
        param.custom_box#set_border_width 4;
        wf#add param.custom_box#coerce;
        wf#coerce
  in
  object (self)
    method box = top
    method apply = param.custom_f_apply ()
  end

(** This class creates a configuration box from a configuration structure *)
class configuration_box conf_struct current_section =

  let main_box = GPack.hbox () in

  let columns = new GTree.column_list in
  let icon_col = columns#add GtkStock.conv in
  let label_col = columns#add Gobject.Data.string in
  let box_col = columns#add Gobject.Data.caml in
  let () = columns#lock () in

  let pane = GPack.paned `HORIZONTAL ~packing:main_box#add () in

  (* Tree view part *)
  let scroll = GBin.scrolled_window ~hpolicy:`NEVER ~packing:pane#pack1 () in
  let tree = GTree.tree_store columns in
  let view = GTree.view ~model:tree ~headers_visible:false ~packing:scroll#add_with_viewport () in
  let selection = view#selection in
  let _ = selection#set_mode `SINGLE in

  let menu_box = GPack.vbox ~packing:pane#pack2 () in

  let renderer = (GTree.cell_renderer_pixbuf [], ["stock-id", icon_col]) in
  let col = GTree.view_column ~renderer () in
  let _ = view#append_column col in

  let renderer = (GTree.cell_renderer_text [], ["text", label_col]) in
  let col = GTree.view_column ~renderer () in
  let _ = view#append_column col in

  let make_param (main_box : #GPack.box) = function
  | Custom_param p ->
    let box = new custom_param_box p in
    let _ = main_box#pack ~expand: p.custom_expand ~padding: 2 box#box in
    box
  in

  let set_icon iter = function
  | None -> ()
  | Some icon -> tree#set ~row:iter ~column:icon_col icon
  in

  (* Populate the tree *)

  let rec make_tree iter conf_struct =
    (* box is not shown at first *)
    let box = GPack.vbox ~packing:(menu_box#pack ~expand:true) ~show:false () in
    box#set_margin_left 4;
    let new_iter = match iter with
    | None -> tree#append ()
    | Some parent -> tree#append ~parent ()
    in
    match conf_struct with Section (label, icon, param_list, children) ->
      let params = List.map (make_param box) param_list in
      let widget =
        object
          method box = box#coerce
          method apply () = List.iter (fun param -> param#apply) params
        end
      in
      let () = tree#set ~row:new_iter ~column:label_col label in
      let () = set_icon new_iter icon in
      let () = tree#set ~row:new_iter ~column:box_col widget in
      List.iter (make_tree (Some new_iter)) children
  in

  let () = List.iter (make_tree None) conf_struct in

  (* Dealing with signals *)

  let current_prop : widget option ref = ref None in

  let select_path path =
    (match !current_prop with
    | None -> ()
    | Some box -> box#box#misc#hide ()
    );
    current_section := Some (GTree.Path.get_indices path);
    let box = tree#get ~row:(tree#get_iter path) ~column:box_col in
    let () = box#box#misc#show () in
    current_prop := Some box
  in

  let when_selected () =
    match selection#get_selected_rows with
    | [] -> ()
    | row :: _ -> select_path row
  in

  (* Focus on a box when selected *)
  let _ = selection#connect#changed ~callback:when_selected in

  let () =
    view#expand_all ();
    Option.iter (fun path ->
        selection#select_path (GTree.Path.create (Array.to_list path));
      ) !current_section;
  in

  object

    method box = main_box

    method apply =
      let foreach _ iter =
        let widget = tree#get ~row:iter ~column:box_col in
        widget#apply(); false
      in
      tree#foreach foreach

  end

(** This function takes a configuration structure list and creates a window
   to configure the various parameters. *)
let edit ?(with_apply=true)
    ?(apply=(fun () -> ()))
    title ?parent ?width ?height ?(current_section = ref None)
    conf_struct =
  let dialog = GWindow.dialog
    ~position:`CENTER
    ~modal: true ~title: title
    ~type_hint:`DIALOG
    ?parent ?height ?width
    ()
  in
  let config_box = new configuration_box conf_struct current_section in

  let _ = dialog#vbox#pack ~expand:true config_box#box#coerce in

  if with_apply then
    dialog#add_button_stock Configwin_messages.mApply `APPLY;
  dialog#add_button_stock Configwin_messages.mOk `OK;
  dialog#add_button_stock Configwin_messages.mCancel `CANCEL;

  let rec iter rep =
    try
      match dialog#run () with
        | `APPLY  -> config_box#apply; iter Return_apply
        | `OK -> config_box#apply; dialog#destroy (); Return_ok
        | _ -> dialog#destroy (); rep
    with
        Failure s ->
          GToolbox.message_box ~title:"Error" s; iter rep
      | e ->
          GToolbox.message_box ~title:"Error" (Printexc.to_string e); iter rep
  in
    iter Return_cancel

(** Create a custom param.*)
let custom ?label box f expand =
  Custom_param
    {
      custom_box = box ;
      custom_f_apply = f ;
      custom_expand = expand ;
      custom_framed = label ;
    }

(* Copying lablgtk question_box + forbidding hiding *)

let question_box ~title ~buttons ?(default=1) ?icon ?parent message =
  let button_nb = ref 0 in
  let window = GWindow.dialog ~position:`CENTER ~modal:true ?parent ~type_hint:`DIALOG ~title () in
  let hbox = GPack.hbox ~border_width:10 ~packing:window#vbox#add () in
  let bbox = window#action_area in
  begin match icon with
    None -> ()
  | Some i -> hbox#pack i#coerce ~padding:4
  end;
  ignore (GMisc.label ~text: message ~packing: hbox#add ());
  (* the function called to create each button by iterating *)
  let rec iter_buttons n = function
      [] ->
        ()
    | button_label :: q ->
        let b = GButton.button ~label: button_label
            ~packing:(bbox#pack ~expand:true ~padding:4) ()
        in
        ignore (b#connect#clicked ~callback:
          (fun () -> button_nb := n; window#destroy ()));
        (* If it's the first button then give it the focus *)
        if n = default then b#grab_default () else ();

        iter_buttons (n+1) q
  in
  iter_buttons 1 buttons;
  ignore (window#connect#destroy ~callback: GMain.Main.quit);
  window#set_position `CENTER;
  window#show ();
  GMain.Main.main ();
  !button_nb

let message_box ~title ?icon ?parent ?(ok="Ok") message =
  ignore (question_box ?icon ?parent ~title message ~buttons:[ ok ])
