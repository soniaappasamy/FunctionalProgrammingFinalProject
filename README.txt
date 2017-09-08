CS3110 Final Project

Notes On Running Our Code:
--------------------------

- We have included a make file in our src zip
- To run the server, run ‘make server’
- To create a client, run ‘make client’ (as many clients can be
  created as you would like)
- Note: remember to start the server before creating clients
- Our system can also be run across different systems (to do this,
  within MessengerClient, change “localhost” in line 133 to be the
  IP address of the computer running the server); If the computer
  running the server falls asleep, the reader and writer channels
  between the clients and server will temporarily close, but the
  server machine must only be woken up to re-establish the connection.
- Note: for any area where you are inputting text into a client
  application,  enter/return on your keyboard can be clicked for
  ease of use instead of physically pressing the send or enter
  button on our GUI
