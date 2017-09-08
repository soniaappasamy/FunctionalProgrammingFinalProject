(*** Server of our system ***)

open Structures
open Async

(* a data element containing all of the groups stored
 * on the server
 *)
val server_messages : (ServerMessagesTree.t) ref

(* adds a new message (in a json string format) to a current
 * group in server_messages, or creates a new group with that
 * message if none currently exist with the people involved
 * in the message
 *)
val add_message_to_server : message -> unit

(* a request from the user for all of their groups and messages;
 * returns a string (in a json format) containing a list of
 * groups
 *)
val get_messages_for_user : user -> message list