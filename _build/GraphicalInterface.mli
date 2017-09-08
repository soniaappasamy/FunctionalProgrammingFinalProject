(*** Client user interface of our system ***)
open Structures

(* [set_id new_id] sets the id to [new_id]
 *)
val set_id : string -> unit

(* [send_pressed] returns the contents of send_button_pressed 
 * (true or false)
 *)
val send_pressed : unit -> bool

(* [not_send_pressed] sets the contents of send_button_pressed
 * to false
 *)
val not_send_pressed : unit -> unit

(* [get_send_msg] returns the contents of curr_send_msg
 *)
val get_send_msg : unit -> string

(* [get_new_msg] returns the contents of new_send_msg
 *)
val get_new_msg : unit -> message

(* [get_end] returns the contents of end_val
 *)
val get_end : unit -> bool

(* [login_mesage msg] sets the text of the text view to the correct
 * question [msg] as part of the login process 
 * requires:
 * - [msg] is the message from the server
 *)
val login_message : string -> unit

(* [main] runs the Gtk main loop
 *)
val main : unit -> unit

(* [update_gui lst] takes all the messages in lst and adds 
 * all the messages to the GUI in the respective tabs in the notebook 
 * requires -
 * [lst] is the list of messages to add to the GUI
 *)
val update_gui : message list -> unit

(* [create_notebook] creates a new container and adds the username label, 
 * notebook (which holds all the conversations in separate tabs), 
 * and button to start a new conversation
 *)
val create_notebook : unit -> unit