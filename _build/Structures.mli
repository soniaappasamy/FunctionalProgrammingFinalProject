(*** Data structures/types of our system ***)

(* [user] is our data structure for a user in our messaging
 * app system.
 *)
type user = string

(* [message] is our data structure for a message in our
 * messaging app system. A message contains a sender,
 * a list of recipients (a list of users), and the
 * actual contents of the message.
 *)
type message = {
    sender : user;
    recipients : user list;
    contents : string;
}

(* [string_to_message str usr lst] creates a new message
 * record from the values in [str], [usr], and [lst]
 * requires:
 * - [str] is the contents of the message
 * - [usr] is the sender of the message
 * - [lst] is the recipients list of the message
 *)
val string_to_message : string -> user -> user list -> message

(* [json_string_to_message str] converts the string [str] to a
 * json object, and then into a message record which is returned.
 *)
val json_string_to_message : string -> message

(* [message_to_json_string msg] creates a message in a json
 * string format from the message [msg].
 *)
val message_to_json_string : message -> string

(* [message_list_to_json_string lst] creates a string in a json
 * string format from the message list [lst].
 *)
val message_list_to_json_string : message list -> string

(* [json_string_to_message_list str] creates a message list
 * from the string [str], which is in a json string format.
 *)
val json_string_to_message_list : string -> message list

module type ServerTree = sig

  type t

  val empty: unit -> t

  val add_msg: user -> message -> t -> string -> t

  val add_usr: user -> t -> t

  val remove: string -> t -> t

  val get_msgs_for_user: user -> t -> message list * ((user list * ((message list) ref)) list)

end

module ServerMessagesTree : ServerTree