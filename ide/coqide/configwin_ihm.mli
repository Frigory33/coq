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

open Configwin_types

val custom : ?label: string -> GPack.box -> (unit -> unit) -> bool -> parameter_kind

val edit :
  ?with_apply:bool ->
  ?apply:(unit -> unit) ->
  string ->
  ?parent:GWindow.window ->
  ?width:int ->
  ?height:int ->
  ?current_section:int array option ref ->
  configuration_structure list ->
  return_button

val question_box : title:string ->
  buttons:string list ->
  ?default:int -> ?icon:#GObj.widget ->
  ?parent:GWindow.window -> string -> int

val message_box :
  title:string -> ?icon:#GObj.widget ->
  ?parent:GWindow.window -> ?ok:string -> string -> unit
