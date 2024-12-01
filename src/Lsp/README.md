framls
======

`framls` is a language server for Fram. This document is meant to be
an entry point for someone wanting to contribute to `framls`.
If you want to configure `framls` for use with your code editor please visit
[fram-lang.org](https://fram-lang.org).

Building
--------

`framls` is built together with DBL when running `dune build` and is
available as `framls` binary.

Basics
------

In LSP, client communicates with the server by sending JSON-RPC messages over
a Base protocol.

A simple diagram is shown below.
```
                             Text/JSON ┆ OCaml
                                       ┆
                                  ┌────┴─────┐
                                  │Message.ml│
                                  └──↑─┬─↑───┘
                                     | ┆ |  
┌──────┐    ┌───────────────┐    ┌───↓─┼─↓────┐    ┌───────────┐    ┌───┐
│Client│<-->│     Base      │<-->│  JSON-RPC  │<-->│  framls   │<-->│DBL│
└──────┘    │(Connection.ml)│    │(JsonRpc.ml)│    │(framls.ml)│    └───┘
            └───────────────┘    └─────┼──────┘    └───────────┘
                                       ┆ 
                                   
```

Below is a basic explanation of relevant protocols/modules. 
Each module also contains a doc comment explaining more or less the same.

Useful links are the
[JSON-RPC specification](https://www.jsonrpc.org/specification)
and the
[LSP specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)

Base
----

Base is a simple HTTP-like protocol. It consists of a header and content part,
separated by "\r\n":
```
header1: value1\r\n
header2: value2\r\n
\r\n
[JSON-RPC message]
```

It is implemented in the `Connection` module.

JSON-RPC
--------

JSON-RPC is an RPC protocol using JSON as data format.
```json
{
	"jsonrpc": "2.0",
	"id": 1,
	"method": "textDocument/completion",
	"params": {
		...
	}
}
```

JSON-RPC messages are divided into
* Requests - sent by the client. They require a Response.
* Responses - sent by the server after processing a Request.
* Notifications - depending on the type can be sent by the server or client.
  They work like live events and don't get a Response.

It is implemented in the `JsonRpc` module and uses the `Message` module
to translate between JSON and OCaml. It also contains the main loop of
the server.

framls (module)
---------------

The `framls` module handles incoming messages, communicating with
the DBL typechecker.

Implementing new messages
-------------------------

This section explains how to extend the functionality of the server by
a new message.

1. Extend `Message`
   1. Define the params of the message as a new type
      as explained in the doc comment at the top of the module.
   2. If the message is a
      * Request - add constructors to `request` and `server_result`
        and extend `request_of_message` and `from_server_result`
      * Notification sent by the client -
        add a constructor to `client_notification`
        and extend `notification_of_message`
      * Notification sent by the server -
        add a constructor to `server_notification`
        and extend `method_name_of_notification` and `json_of_notification`
2. Extend `framls` and possibly `State`.
   Add the logic and extend `handle_notification` or `handle_request`.

You should not need to change `JsonRpc` or `Connection`.

Debugging
---------

There is no logging implemented. To see the messages that are being sent
follow these steps:
1. Choose your favorite port number and substitute it for `5555` below.
2. Configure your editor to use `nc localhost 5555` as the language server
for fram.
3. Create two named pipes
```bash
mkfifo in
mkfifo out
```
4. If you want to see outputs of both client and server run
```bash
cat in | framls | tee -p out &
cat out | nc -l 5555 | tee -p in & 
```
5. If you want to see only the output of the server run
```bash
cat in | framls | tee -p out &
cat out | nc -l 5555 > in & 
```
6. Run the editor.
