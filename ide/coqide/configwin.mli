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

(** This module is the interface of the Configwin library. *)

(** {2 Types} *)

(** This type represents the different kinds of parameters. *)
type parameter_kind;;

(** To indicate what button pushed the user when the window is closed. *)
type return_button =
    Return_apply
      (** The user clicked on Apply at least once before
         closing the window with Cancel or the window manager. *)
  | Return_ok
      (** The user closed the window with the ok button. *)
  | Return_cancel
      (** The user closed the window with the cancel
         button or the window manager but never clicked
         on the apply button.*)

(** This type represents the structure of the configuration window. *)
type configuration_structure =
  Section of string * GtkStock.id option * parameter_kind list * configuration_structure list

(** {2 Functions to create parameters} *)

(** [custom box f expand] creates a custom parameter, with
   the given [box], the [f] function is called when the user
   wants to apply his changes, and [expand] indicates if the box
   must expand in its father.
   @param label if a value is specified, a the box is packed into a frame.
*)
val custom : ?label: string -> GPack.box -> (unit -> unit) -> bool -> parameter_kind

(** {2 Utility for modifier configuration} *)

(** The list of all modifiers that may be used for configuration *)
val all_modifiers : Gdk.Tags.modifier list

(** The string representation of a modifier for use in the preferences dialog
   @param modifier the modifier
*)
val string_of_modifier : Gdk.Tags.modifier -> string

(** {2 Functions creating configuration windows and boxes} *)

(** This function takes a configuration structure and creates a window
   to configure the various parameters.
   @param apply this function is called when the apply button is clicked, after
   giving new values to parameters.
*)
val edit :
  ?apply: (unit -> unit) ->
  string ->
  ?parent:GWindow.window ->
  ?width:int ->
  ?height:int ->
  ?current_section:int array option ref ->
  configuration_structure list ->
  return_button
