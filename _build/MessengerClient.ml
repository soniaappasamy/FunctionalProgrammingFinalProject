open Structures
open Core.Std
open Async.Std

(* [main_update r] is a deferred that updates the graphical
 * interface of the client when the server writes a message
 * to the client
 * requires:
 * - [r] is the reader from the server connection
 *)
let rec main_update r =
  if GraphicalInterface.get_end () then
    return ()
  else
    begin
      Reader.read_line r >>= function
      | `Eof -> return r >>= main_update
      | `Ok msg ->
          begin
            let msg_lst = json_string_to_message_list msg in
            GraphicalInterface.update_gui msg_lst;
            return r >>= main_update
          end
    end

(* [main_send w] is a deferred that checks to see if the user
 * has entered a new message into the GUI to be sent to the
 * server, and sends the message if there is one
 * requires:
 * - [w] is the writer from the server connection
 *)
let rec main_send w =
  if GraphicalInterface.get_end () then
    return ()
  else
    begin
      match GraphicalInterface.send_pressed () with
      | false -> (after (Core.Std.sec 0.01))
                    >>= (fun () -> return w)
                    >>= main_send
      | true ->
          begin
            let msg = GraphicalInterface.get_new_msg () in
            let json_msg = message_to_json_string msg in
            GraphicalInterface.not_send_pressed ();
            Writer.write_line w json_msg;
            (after (Core.Std.sec 0.01))
              >>= (fun () -> main_send w)
          end
    end

(* [main_run r w] is a deferred that starts as soon as the
 * client has successfully logged in. It starts the [main_send]
 * and [main_update] functions, which handle sending messages
 * from the GUI, and updating the GUI.
 * requires:
 * - [r] is the reader from the server connection
 * - [w] is the writer from the server connection
 *)
let rec main_run r w =
  GraphicalInterface.create_notebook ();
  ignore (main_send w);
  main_update r

(* [set_username_password msg] sets the username (id) of the user
 * in the GraphicalInterface module
 * requires:
 * - [msg] is "!~id", which has been sent from the server, with id
 *         replaced with the client's actual id (aquired through
 *         server login)
 *)
let set_username_password msg =
  let username_length = (String.length msg) - 2 in
  let username = String.sub msg 2 username_length in
  GraphicalInterface.set_id username

(* [send_response w] is a deferred that is not determined until
 * the user has entered in a text response to a prompting on the
 * GUI. We added a delay in between sending a response to the
 * user so as to not create a stall of the client application
 * (due to running the deferred at the same time as the GUI, in
 * different threads).
 * requires:
 * - [w] is the writer from the server connection
 *)
let rec send_response w =
  match GraphicalInterface.send_pressed () with
  | false -> (after (Core.Std.sec 0.01))
                >>= (fun () -> return w)
                >>= send_response
  | true ->
      begin
        let msg = GraphicalInterface.get_send_msg () in
        GraphicalInterface.not_send_pressed ();
        Writer.write_line w msg;
        return ()
      end

(* [login s r w] is a deferred that handles the login process
 * on the client side. Based on the server reader, it prompts
 * the user to enter in responses, and continues doing this
 * until the login proccess is over (notified by a message
 * sent from the server in the format of "!~id"). After this,
 * it calls the [main_run] deferred, which runs for the rest
 * of the time that the client window is open.
 * requires:
 * - [r] is the reader from the server connection
 * - [w] is the writer from the server connection
 *)
let rec login r w =
  Reader.read_line r >>= function
  | `Eof -> return ()
  | `Ok msg ->
          begin
            let check = String.get msg 0 in
            if (check = '!') then
              begin
                set_username_password msg;
                main_run r w
              end
            else
              begin
                GraphicalInterface.login_message msg;
                (send_response w) >>= (fun () -> login r w)
              end
          end

(* [client ()] is a deferred that creates the connection to
 * the server, using TCP. Once the connection has been
 * established, the login process is started.
 *)
let client () =
  Tcp.with_connection (Tcp.to_host_and_port "localhost" 4311)
  (fun s r w -> login r w)

(* This is the method which physically runs our clients. As
 * seen, our client is running in two separate threads at
 * the same time. The first thread is the deferreds, handled
 * all within MessengerClient (which use the scheduler and
 * acutally connect with the server). The second is the main
 * program thread, which runs the graphical interface. This
 * starts the GUI, but the GUI code is all contained within
 * GraphicalInterface.
 *)
let () =
  let t1 = Thread.create
            begin
              (fun () ->
                begin
                  ignore (client ());
                  never_returns (Scheduler.go ())
                end)
            end () in
  GraphicalInterface.main ()