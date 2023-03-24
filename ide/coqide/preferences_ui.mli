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

(** {2 Preferences structure} *)

type pref_section = {
  label: string;
  box: GPack.box;
  apply: unit -> unit;
}

(** This type represents a category (tab) of the preferences window. *)
type pref_category = {
  label: string;
  icon: GtkStock.id option;
  sections: pref_section list;
  children: pref_category list;
}

(** To indicate what button pushed the user when the window is closed. *)
type return_button =
  | Preferences_Apply
      (** The user clicked on Apply at least once before
          closing the window with Cancel or the window manager. *)
  | Preferences_OK
      (** The user closed the window with the OK button. *)
  | Preferences_Cancel
      (** The user closed the window with the Cancel
          button or the window manager but never clicked
          on the apply button.*)

val pref_category :
  string -> ?icon: GtkStock.id -> ?children: pref_category list -> pref_section list ->
  pref_category

(** {2 Functions creating preferences windows and boxes} *)

(** This function takes a preferences structure and creates a window
   to configure the various parameters.
   @param apply this function is called when the apply button is clicked, after
   giving new values to parameters.
*)
val edit_preferences :
  ?apply: (unit -> unit) ->
  string ->
  ?parent: GWindow.window -> ?width: int -> ?height: int ->
  ?current_section: int array option ref ->
  pref_category list ->
  return_button

(** This functions shows the main preferences configuration dialog. *)
val configure : ?apply: (unit -> unit) -> GWindow.window -> unit

(** {2 Utilities to use simple message boxes} *)

type button_contents =
  | ButtonUseStock of GtkStock.id
  | ButtonUseString of string

val question_box :
  ?parent:GWindow.window -> ?icon:#GObj.widget -> title:string ->
  ?buttons:button_contents list -> ?default:int -> string -> int

val message_box :
  ?parent:GWindow.window -> ?icon:#GObj.widget -> title:string ->
  ?ok:button_contents -> string -> unit
