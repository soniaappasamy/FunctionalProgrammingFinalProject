open GMain
open GdkKeysyms
open GtkThread
open Structures

let _ = GMain.init ()

let client_msg_lst = ref []
let id = ref ""
let send_button_pressed = ref false
let curr_send_msg = ref ""
let new_send_msg = ref {sender="";recipients=[];contents=""}
let curr_recip = ref []
let end_val = ref false

let window = GWindow.window ~title:"Messaging Application" 
                            ~border_width:10 ()
let frame = GBin.frame ~label:"LOGIN SCREEN" ~width:300 
                       ~packing:window#add ()
let vbox = GPack.vbox ~packing:frame#add ()
let textview = GText.view ~cursor_visible: false ~width:200 ~height:40
                          ~editable:false ~packing:vbox#add ()
let entry = GEdit.entry ~max_length:500 ~packing:vbox#add ()
let button = GButton.button ~label:"Enter" ~packing:vbox#add ()
let vbox_scroll = ref []
let notebook = ref (GPack.notebook ())

(* [set_id new_id] sets the id to [new_id] *)
let set_id new_id =
  id := new_id

(* [send_pressed] returns the contents of send_button_pressed 
 * (true or false) *)
let send_pressed () =
  !send_button_pressed

(* [not_send_pressed] sets the contents of send_button_pressed
 * to false *)
let not_send_pressed () =
  send_button_pressed := false

(* [get_send_msg] returns the contents of curr_send_msg *)
let get_send_msg () =
  !curr_send_msg

(* [get_new_msg] returns the contents of new_send_msg *)
let get_new_msg () =
  !new_send_msg

(* [get_end] returns the contents of end_val *)
let get_end () =
  !end_val


(* [send_message entry vbox_scroll] populates the frame with the message
 * entered by the user
 * requires -
 * [entry] is the entry field where the user was asked to type the
 *         contents of the message
 * [vbox_scroll] is the container for all the messages in the conversation
 *               between the user and the recipient(s) *)
let send_message entry vbox_scroll () =
  let text = entry#text in
  curr_send_msg := text;
  new_send_msg := {sender=(!id);recipients=(!curr_recip);contents=text};
  send_button_pressed := true;

  let frame = GBin.frame ~label:("me:") ~label_xalign:0.0 ~height:40 
    ~shadow_type:`ETCHED_OUT ~packing:vbox_scroll#add () in
  ignore (GMisc.label ~text:text ~justify:`LEFT ~packing:frame#add ());

  entry#set_text ("");
  flush stdout


(* [scroll_adjust adj] sets the value of adj, the adjustment for the
 * scroll bar of the scrolled window which contains the messages*)
let scroll_adjust adj () =
  adj#set_value (adj#upper -. adj#page_size)


(* [new_tab recip_text] creates a new tab for a new conversation in 
 * the notebook. It initializes a new container to contain all the messages 
 * in the conversation, the entry field to write the message, and a button 
 * to send the message 
 * requires -
 * [recip_text] is the name of the recipient(s) which was entered in the
 *              dialog box *)
let new_tab recip_text =
  let label = GMisc.label ~text:recip_text () in
  let temp = (fun x -> ignore(!notebook#insert_page 
                                ~tab_label:label#coerce x ~pos:2)) in
  let frame = GBin.frame ~width:100 ~height:325 
                                ~border_width:10 ~packing:temp () in
  let vbox = GPack.vbox ~packing:frame#add () in

  let adj = GData.adjustment () in
  ignore (adj#connect#changed ~callback:(scroll_adjust adj));

  let scrolled_window = GBin.scrolled_window ~hpolicy:`AUTOMATIC 
    ~vadjustment:adj ~height:250 ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let temp = GPack.vbox ~packing:scrolled_window#add_with_viewport () in
  vbox_scroll := (!vbox_scroll)@[temp];

  let entry = GEdit.entry ~text:"Enter your message here" ~max_length:500 
    ~packing:vbox#add () in
  ignore (entry#connect#activate ~callback:(send_message entry temp));

  let button = GButton.button ~label:"Send Message" ~packing:vbox#add () in
  ignore (button#connect#clicked ~callback:(send_message entry temp))


(* [create_new_group_tab recip_text] creates a new tab in 
 * the notebook for a new conversation with the given recipients, 
 * and creates a string list of the recipients and updates the list 
 * of messages for the client 
 * requires - 
 * [recip_text] is the string containing the recipients of the message *)
let create_new_group_tab recip_text =
  new_tab recip_text;
  let rcvr_user_list = Str.split (Str.regexp "[, \t]+") recip_text in
  let rcvr_user_list = (!id)::rcvr_user_list in
  client_msg_lst := !client_msg_lst@[(rcvr_user_list,ref [])]

(* [recips lst] returns a list of all the members in the conversation
 * excluding the client 
 * requires -
 * [lst] is the list of members of the conversation *)
let rec recips lst =
  match lst with
  | [] -> []
  | _::[] -> []
  | h::t ->
      begin
        if h = !id then
          t
        else
          h::(recips t)
      end

(* [create_recip_texts usrs] creates a string of all the elements
 * of [usrs] (the recipients) concatenated by commas 
 * requires - 
 * [usrs] is the list of recipients *)
let rec create_recip_text usrs =
  match usrs with
  | [] -> ""
  | h::[] -> h
  | h::t -> h^", "^(create_recip_text t)


(* [create_new_group members msg] updates client_msg_lst and creates
 * a new tab in the notebook for the conversation *)
let create_new_group members msg =
  client_msg_lst := (!client_msg_lst)@[(members,ref [msg])];
  let other_members = recips members in
  let recip_text = create_recip_text other_members in
  new_tab recip_text


(* [compare_helper lst1 lst2] checks if the elements of lst1 and lst2
 * are the same, regardless of the order of the elements. *)
let rec compare_helper lst1 lst2 =
  match lst1 with
  | [] -> true
  | h::t ->
      begin
        (List.mem h lst2) && (compare_helper t lst2)
      end

(* [compare lst1 lst2] returns true if lst1 and lst2 contain the same
 * elements (regardless of the order of the elements), and returns
 * false otherwise *)
let compare lst1 lst2 =
  if (List.length lst1 = List.length lst2) then
    compare_helper lst1 lst2
  else
    false


(* [get_page_index index mems lst] returns the index of the tab 
 * that is currently selected, as it is ordered in the notebook 
 * containing all the conversations *)
let rec get_page_index index mems lst =
  match lst with
  | [] -> index
  | (h,_)::t ->
        begin
          if compare h mems then
            index
          else
            get_page_index (index+1) mems t
        end


(* [update_gui_helper lst curr_group curr_tab] is a helper function 
 * to add all the messages in lst to the GUI, in the respective tabs 
 * according to the members of the conversation. If a tab does not exist,
 * it creates a new tab with the curr_group. Otherwise, it updates the 
 * existing tab and adds the new messages entered by the members 
 * of the group 
 * requires -
 * [lst] is the list of messages to add to the GUI 
 * [curr_group] is the current group of members
 * [curr_tab] is the currently selected tab in the notebook *)
let rec update_gui_helper lst curr_group curr_tab =
  match lst with
  | [] -> ()
  | h::t ->
      begin
        let members = h.sender::h.recipients in
        let check = List.filter (fun (x,_) -> 
          compare members x) !client_msg_lst in
        match List.length check with
        | 0 ->
            begin
              create_new_group members h;
              let page_index = get_page_index 0 members !client_msg_lst in
              let curr_vbox = List.nth !vbox_scroll page_index in
              let label = if h.sender = !id then "me:" else (h.sender^":") in
              let frame = GBin.frame ~label:label ~label_xalign:0.0 ~height:40 
                ~shadow_type:`ETCHED_OUT ~packing:curr_vbox#add () in
              ignore (GMisc.label ~text:h.contents ~justify:`LEFT 
                          ~packing:frame#add ());
              update_gui_helper t curr_group curr_tab
            end
        | _ ->
            begin
              let (_, msg_lst) = List.nth check 0 in
              msg_lst := h::(!msg_lst);
              let page_index = get_page_index 0 members !client_msg_lst in
              let curr_vbox = List.nth !vbox_scroll page_index in
              let label = if h.sender = !id then "me:" else (h.sender^":") in
              let frame = GBin.frame ~label:label ~label_xalign:0.0 ~height:40 
                ~shadow_type:`ETCHED_OUT ~packing:curr_vbox#add () in
              ignore (GMisc.label ~text:h.contents ~justify:`LEFT 
                          ~packing:frame#add ());
              update_gui_helper t curr_group curr_tab
            end
      end

(* [update_gui lst] takes all the messages in lst and adds 
 * all the messages to the GUI in the respective tabs in the notebook 
 * requires -
 * [lst] is the list of messages to add to the GUI *)
let update_gui lst =
  let tab = !notebook#current_page in
  if (tab <> -1) then
    begin
      let (curr_group,_) = List.nth !client_msg_lst !notebook#current_page in
      curr_recip := recips curr_group;
      update_gui_helper lst curr_group tab
    end
  else if (lst <> []) then
    begin
      let curr_group = 
        ((List.nth lst 0).sender)::((List.nth lst 0).recipients) in
      curr_recip := recips curr_group;
      update_gui_helper lst curr_group 0;
      !notebook#goto_page 0;
    end
  else
    ()

(* [login_mesage msg] sets the text of the text view to the correct
 * question [msg] as part of the login process 
 * requires -
 * [msg] is the message from the server *)
let login_message msg =
  textview#buffer#set_text msg

(* [login_send_message] stores the text that is entered in the 
 * entry field by the user for each stage in the login process 
 * and resets the entry field *)
let login_send_message () =
  let text = entry#text in
  curr_send_msg := text;
  send_button_pressed := true;
  entry#set_text ""


(* [create] creates a dialog box with an entry field 
 * to enter the recipients of the message. *) 
let create () =
  let dialog_box = GWindow.dialog 
    ~title:"Enter Recipients (separated by ', ' eg. username1, username2)" 
    ~width:500 ~position:`CENTER_ON_PARENT ~show:true () in
  let enter_text = GEdit.entry ~max_length:100 
    ~packing:dialog_box#vbox#add () in
  let enter_button = GButton.button ~label:"Enter" 
    ~packing:dialog_box#action_area#add () in
  ignore (enter_button#connect#clicked ~callback:(fun () -> 
    create_new_group_tab enter_text#text; dialog_box#destroy ()));
  ignore (enter_text#connect#activate ~callback:(fun () -> 
    create_new_group_tab enter_text#text; dialog_box#destroy ()))


(* [create_notebook] creates a new container and adds the username label,
 * notebook (which holds all the conversations in separate tabs), 
 * and button to start a new conversation *)
let create_notebook () =
  frame#destroy ();
  window#resize ~width:600 ~height:450;
  let vbox_book = GPack.vbox ~packing:window#add () in 

  let _ = GMisc.label ~text:("My Username: "^(!id)) 
    ~packing:vbox_book#add () in
  notebook := (GPack.notebook ~tab_pos:`LEFT 
      ~packing:(vbox_book#add) ~show:true ());
  let new_button = GButton.button ~label:"New Conversation" 
    ~packing:(vbox_book#add) () in
  ignore (new_button#connect#clicked ~callback:(create));
  ()

(*[main] runs the Gtk main loop*)
let main () =
  ignore (window#connect#destroy 
    ~callback:(fun x -> end_val := true; GMain.Main.quit x));

  ignore (button#connect#clicked ~callback:(login_send_message));
  ignore (entry#connect#activate ~callback:(login_send_message));

  GtkThread.async window#show ();
  GtkThread.main ()