open Yojson
open Structures
open Core.Std
open Async.Std

(* [user_id_list] is a pointer to the association list mapping
 * usernames to respective passwords
 *)
let user_id_list = ref []

(* [server_messages] is the pointer to the server storage *)
let server_messages = ref (ServerMessagesTree.empty ())

(* [add_to_messages recipients msg] modifies server_messages such that
 * every user in the [recipients] list has the message [msg] added to
 * its list of messages
 * requires:
 * - [recipients] is the user list corresponding to recipients of [msg]
 * - [msg] is of type message
 *)
let rec add_to_messages recipients msg =
  match recipients with
  | [] -> (
    server_messages :=
    ServerMessagesTree.add_msg msg.sender msg !server_messages "history"
  )
  | h::t ->
      begin
        server_messages :=
        ServerMessagesTree.add_msg h msg !server_messages "messages";
        add_to_messages t msg
      end

(* [add_message_to_server msg] applies [add_to_messages] to the list of
 * recipients in message [msg] such that [msg] is added to the list of
 * messages for all its recipients
* requires:
 * - [msg] is of type message
 *)
let add_message_to_server msg =
  let recipients = msg.recipients in
  add_to_messages recipients msg

(* [get_messages_for_user usr] is the list of pending messages in server
 * for user [usr]
 * requires:
 * - [usr] is of type user
 *)
let get_messages_for_user usr =
  fst (ServerMessagesTree.get_msgs_for_user usr !server_messages)

(* [get_msg_hist usr] is the chat history for user [usr]
 * requires:
 * - [usr] is of type user
 *)
let get_msg_hist usr =
  snd (ServerMessagesTree.get_msgs_for_user usr !server_messages)

(* [new_send r] continuously reads on reader [r] for an input json string
 * containing details of a message, and converts the string to a message and
 * adds that message to server storage
 * requires:
 * - [r] is the reader from the server connection
 *)
let rec new_send r =
  Reader.read_line r >>= function
  | `Eof -> return ()
  | `Ok com ->
      begin
        let msg = json_string_to_message com in
        add_message_to_server msg;
        new_send r
      end

(* [request_message usr w] is a deferred handling requesting messages for
 * user [usr[ from the server and writing a json string from the message list
 * on writer [w] for the client application to read. It is run on a constant
 * loop until [w] is closed to ensure continuous retrieval of messages when an
 * active connection exists
 * requires:
 * - [usr] is of type user
 * - [w] is the writer from the server connection
 *)
let rec request_message usr w =
  if Writer.is_closed w
  then return ()
  else (
    let msg_list = get_messages_for_user usr in
    let string_msg = message_list_to_json_string msg_list in
    Writer.write_line w string_msg;
    (after (Core.Std.sec 0.01)) >>= fun () -> request_message usr w
  )

(* [acc_helper l1 l2] is used to append two lists [l1] and [l2]
 * tail recursively
 *)
let acc_helper l1 l2 =
  List.fold_left l1 ~init:l2 ~f:(fun acc x -> x :: acc)

(* [request_msg_history usr w] is a unit deferred that handles retrieving
 * the chat history for user [usr] and writes it to writer [w] after
 * converting the message list to a json string for the client application
 * to read.
 * requires:
 * - [usr] is of type user
 * - [w] is the writer from the server connection
 *)
let request_msg_history usr w =
  if Writer.is_closed w
  then return ()
  else (
    let msg_list = List.rev (get_msg_hist usr) in
    let msgs =
    List.fold_left msg_list ~init:[] ~f:(fun acc (_,m) -> acc_helper !m acc)
    in
    let string_msg = message_list_to_json_string msgs in
    Writer.write_line w string_msg;
    return ()
  )

(* [send_and_request] handles calling [request_msg_history],
 * [request_message] and [new_send] in the correct order upon establishing
 * an active connection with the client and passing in the reader and writer
 * created on the socket
 * requires:
 * - [usr] is of type user
 * - [r] is the reader from the server connection
 * - [w] is the writer from the server connection
 *)
let send_and_request usr r w =
  ignore (request_msg_history usr w);
  ignore (request_message usr w);
  new_send r

(* [try_login] is responsible for reading client input for a login
 * attempt. Once the client is logged in, [send_and_request] is called
 * with the username and the reader and writer on the socket passed in
 * as arguments so that the client application can begin sending and receiving
 * messages.
 * requires:
 * - [r] is the reader from the server connection
 * - [w] is the writer from the server connection
 *)
let rec try_login r w =
  Writer.write_line w "Enter username: ";
  Reader.read_line r >>= function
  | `Eof -> return ()
  | `Ok username ->(
    match List.Assoc.find !user_id_list username with
    | Some pswrd -> (
      Writer.write_line w "Enter password: ";
      Reader.read_line r >>= function
      | `Eof -> try_login r w
      | `Ok password ->(
        if pswrd = password
        then (
          Writer.write_line w ("!~"^username);
          send_and_request username r w
        )
        else (
          Writer.write w "Incorrect password! Try again! ";
          try_login r w
        )
      )
    )
    | None -> (
      Writer.write w "Username not found! Try again! ";
      try_login r w
      )
  )

(* [make_login] is responsible for creating a new login for the client
 * by reading client input. Once a login has been created, [send_and_request]
 * is called to enable the client application to send and receive messages
 * using the reader and writer created on the socket when the connection was
 * established.
 * requires:
 * - [r] is the reader from the server connection
 * - [w] is the writer from the server connection
 *)
let rec make_login r w =
  Writer.write_line w "Enter username: ";
  Reader.read_line r >>= function
  | `Eof -> return ()
  | `Ok username ->
      begin
        if List.Assoc.mem !user_id_list username then
          begin
            Writer.write w "Username already taken! ";
            make_login r w
          end
        else
          begin
            Writer.write_line w "Enter password:";
            Reader.read_line r >>= function
            | `Eof -> return ()
            | `Ok password ->
                begin
                  user_id_list :=
                    List.Assoc.add !user_id_list username password;
                  server_messages :=
                    ServerMessagesTree.add_usr username !server_messages;
                  Writer.write_line w ("!~"^username);
                  send_and_request username r w
                end
          end
      end

(* [handler] is the function called when a connection is established on
 * the server. It is responsible for reading client input using the reader
 * and writer created on the socket for whether or not a login for the user
 * exists, and then calling [make_login] or [try_login] appropriately
 * requires:
 * - [r] is the reader from the server connection
 * - [w] is the writer from the server connection
 *)
let rec handler r w =
  Writer.write_line w "Are you an existing user (Y/N):";
  Reader.read_line r >>= function
  | `Eof -> return ()
  | `Ok exist ->
      begin
        let existing = String.lowercase exist in
        if existing = "y" then try_login r w
        else if existing = "n" then make_login r w
        else let () = Writer.write w "Invalid input! " in handler r w
      end

(* [server ()] creates a tcp server that listens on
 * port 4311 for communication with the client, calling [handler] when a
 * connection is established and a reader and writer are created on the
 * socket
 *)
let server () =
  Tcp.Server.create (Tcp.on_port 4311) (fun _ r w -> handler r w)

let () =
  ignore (server ());
  never_returns (Scheduler.go ())