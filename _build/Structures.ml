open Yojson

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

(* [get_json_member member json] is the json member with the
 * name [member] in the json object [json].
 *)
let get_json_member member json =
    Yojson.Basic.Util.member member json

(* [get_json_string member json] is the string in the json
 * object [json] at the member [member].
 *)
let get_json_string member json =
    Yojson.Basic.Util.to_string (get_json_member member json)

(* [get_json_list member json] is a list of json objects found
 * in the json object [json] at the member [member].
 *)
let get_json_list member json =
    Yojson.Basic.Util.to_list (get_json_member member json)

(* [get_recip_from_json json] gets the user value from a
 * json object
 * requires:
 * - [json] must be in the format {"to":"username"}
 *)
let get_recip_from_json json =
    get_json_string "to" json

(* [string_to_message str usr lst] creates a new message
 * record from the values in [str], [usr], and [lst]
 * requires:
 * - [str] is the contents of the message
 * - [usr] is the sender of the message
 * - [lst] is the recipients list of the message
 *)
let string_to_message str usr lst =
    {sender = usr; recipients = lst; contents = str}

(* [json_object_to_message json] goes through the json object [json]
 * and creates a new record message from its contents.
 *)
let json_object_to_message json =
    let sender = get_json_string "sender" json in
    let recip_lst = get_json_list "recipients" json in
    let recipients = List.map (fun x -> get_recip_from_json x) recip_lst in
    let contents = get_json_string "contents" json in
    {sender = sender; recipients = recipients; contents = contents}

(* [json_string_to_message str] converts the string [str] to a
 * json object, and then into a message record which is returned.
 *)
let json_string_to_message str =
    let json = Yojson.Basic.from_string str in
    json_object_to_message json

(* [recipients_list lst] creates a string in a json
 * string format from the recipients list [lst].
 *)
let rec recipients_list lst =
    match lst with
    | [] -> ""
    | h::[] -> "{\"to\":\""^h^"\"}"
    | h::t -> "{\"to\":\""^h^"\"},"^(recipients_list t)

(* [message_to_json_string msg] creates a message in a json
 * string format from the message [msg].
 *)
let message_to_json_string msg =
    let message = "{\"sender\":\""^msg.sender^"\"," in
    let message = message^"\"recipients\":[" in
    let message = message^(recipients_list msg.recipients) in
    let message = message^"],\"contents\":\""^msg.contents^"\"}" in
    message

(* [json_list lst] creates a string in a json
 * string format from the list [lst].
 *)
let rec json_list lst =
    match lst with
    | [] -> "]}"
    | h::[] -> h^"]}"
    | h::t -> h^","^(json_list t)

(* [message_list_to_json_string lst] creates a string in a json
 * string format from the message list [lst].
 *)
let message_list_to_json_string lst =
    let string_lst = List.map message_to_json_string lst in
    let send = "{\"messages\":[" in
    send^(json_list string_lst)

(* [json_string_to_message_list str] creates a message list
 * from the string [str], which is in a json string format.
 *)
let json_string_to_message_list str =
    let json = Yojson.Basic.from_string str in
    let lst = get_json_list "messages" json in
    List.map json_object_to_message lst

module type ServerTree = sig
  type t
  val empty: unit -> t
  val add_msg: user -> message -> t -> string -> t
  val add_usr: user -> t -> t
  val remove: string -> t -> t
  val get_msgs_for_user: user -> t ->
            message list * ((user list * ((message list) ref)) list)
end

module ServerMessagesTree : ServerTree = struct

  (* A binary search tree holding the user, messages to be delivered, and
   * the user's chat history at each node. The username is used for comparison.
   *)
  type t = Leaf | Node of (user * ((message list) ref)
                          * (user list * ((message list) ref)) list ref
                          * t * t)

  (* Creates an empty value of type t, i.e an empty binary search tree *)
  let empty () = Leaf

  (* [first_fifty l] is a list of the first fifty elements in list [l]
   * requires:
   * - [l] is a list containing at least 50 elements
   *)
  let first_fifty l =
    let acc = ref [] in
    let n = ref 49 in
    while !n >= 0 do
      acc := (List.nth l !n) :: !acc;
      n := !n - 1
    done;
    !acc

  (* [add_msg_to_hist msg msg_hist] adds message [msg] based on the sender of
   * the message to the chat history to which [msg_hist] is a pointer.
   * [send_or_receive] is used to determine the key to the value [msg] has to be
   * added to.
   * requires:
   * - [msg] is of type message
   * - [msg_hist] is a pointer to an association list with users as keys and
   *   a pointer to a message list
   * - [send_or_receive] is a string
   *)
  let add_msg_to_hist msg msg_hist send_or_receive =
    if send_or_receive = "send"
    then (
      if List.mem_assoc [msg.sender] !msg_hist
      then
        begin
          let msgs = List.assoc [msg.sender] !msg_hist in
          let () = msgs := msg :: !msgs in
          if List.length (!msgs) > 50
          then msgs := first_fifty !msgs
          else ()
        end
      else msg_hist := ([msg.sender],ref [msg]) :: !msg_hist
    )
    else (
      if List.mem_assoc msg.recipients !msg_hist
      then
        begin
          let msgs = List.assoc msg.recipients !msg_hist in
          let () = msgs := msg :: !msgs in
          if List.length (!msgs) > 50
          then msgs := first_fifty !msgs
          else ()
        end
      else msg_hist := (msg.recipients,ref [msg]) :: !msg_hist
    )

  (* [add_msg usr msg tree to_where] is the value of type t after adding
   * message [msg] to either chat history or messages to be delivered of
   * user [usr] in t [tree] based on string [to_where]. If to_where = "history"
   * then the message is added to the chat history otherwise it is added to
   * messages to be delivered.
   * requires:
   * - [usr] is of type user
   * - [msg] is of type message
   * - [tree] is of type t
   * - [to_where] is a string
   *)
  let rec add_msg usr msg tree to_where =
    match tree with
    | Leaf -> tree
    | Node (u,msgs,msg_hist,l,r) ->
      begin
        if usr = u then
          begin
            if to_where = "history"
            then let () = add_msg_to_hist msg msg_hist "receive" in tree
            else let () = msgs := msg :: !msgs in
            tree
          end
        else
          begin
            if usr > u then
              Node (u,msgs,msg_hist,l,add_msg usr msg r to_where)
            else
              Node (u,msgs,msg_hist,add_msg usr msg l to_where,r)
          end
      end

  (* [add_usr usr tree] is the value of type t after user [usr] is added to
   * the value of type t [tree]
   * requires:
   * - [usr] is of type user
   * - [tree] is of type t
   *)
  let rec add_usr usr tree =
    match tree with
    | Leaf -> Node (usr,ref [],ref [],Leaf,Leaf)
    | Node (u,msgs,msg_hist,l,r) ->
        begin
          if usr < u then
            Node (u,msgs,msg_hist,add_usr usr l,r)
          else
            Node (u,msgs,msg_hist,l,add_usr usr r)
        end

  (* [min_value tree] is ((u,msgs),msg_hist) where u is the user, msgs is the
   * list of messages to be delivered, and msg_hist is the chat history of the
   * user with the smallest username according to comparisons in t [tree]
   * requires:
   * - [tree] is of type t and is non-empty
   *)
  let rec min_value tree =
    match tree with
    | Leaf -> failwith "invalid empty"
    | Node (u,msgs,msg_hist,l,_) ->
        begin
          if l = Leaf then
            ((u,msgs),msg_hist)
          else min_value l
        end

  (* [remove usr tree] is the value of type t when the node holding user
   * [usr] is removed from t [tree]
   * requires:
   * - [usr] is of type user
   * - [tree] is of type t and contains a node with user [usr]
   *)
  let rec remove usr tree =
    match tree with
    | Leaf -> failwith "not found"
    | Node (u,messages,msg_hist,l,r) ->
        begin
          if usr = u then
            begin
              if (l = Leaf && r = Leaf) then
                Leaf
              else
                begin
                  if (l = Leaf) then
                    r
                  else
                    begin
                      if (r = Leaf) then
                        l
                      else
                        begin
                          let minimum = min_value r in
                          let user_and_msgs = fst minimum in
                          let msg_history = snd minimum in
                          let user = fst user_and_msgs in
                          let msgs = snd user_and_msgs in
                          let to_remove = user in
                          Node (user,msgs,msg_history,l,remove to_remove r)
                        end
                    end
                end
            end
          else
            begin
              if usr < u then
                Node (u, messages, msg_hist,
                      remove usr l,r)
              else
                Node (u, messages, msg_hist,
                      l, remove usr r)
            end
        end

  (* [get_msgs_for_user usr t] is (msgs,msg_hist) where msgs is the list of
   * pending messages for user [usr] and msg_hist is the chat history for user
   * [usr].
   * requires:
   * - [usr] is of type user
   * - [t] is of type t
   *)
  let rec get_msgs_for_user usr t =
    match t with
    | Leaf -> ([],[])
    | Node (u,msgs,msg_hist,l,r) ->
        begin
          if usr = u then
            begin
              List.iter (fun msg -> add_msg_to_hist msg msg_hist "send")
                        (List.rev !msgs);
              let temp = (!msgs,!msg_hist) in
              msgs := [];
              temp
            end
          else
            begin
              if usr < u then
                get_msgs_for_user usr l
              else
                get_msgs_for_user usr r
            end
        end

end