network
-------

Sketch of an idea for a scheduler-agnostic networking library with structured concurrency.

Very inspired by `Eio.Net` and `Miou_unix`.


### Scheduler-agnostic Networking

<!-- $MDX file=example/net_example.ml -->
```ocaml
(* No dependency on Eio or Miou *)

let loopback = `Tcp (Addr.V4.localhost, 8787)

let listen ~sw response =
  let listener =
    Net.listen ~reuse_addr:true ~reuse_port:true ~backlog:5 ~sw loopback
  in
  Format.printf "Listening on %a\n%!" Addr.pp loopback;
  while true do
    let sock, _ = Net.accept ~sw listener in
    Format.printf "Read: %s\n%!" (Net.read_all sock);
    Net.write sock response;
    Net_unix.with_socket_fd sock @@ fun fd -> Unix.shutdown fd SHUTDOWN_SEND
  done

let connect_and_send ~sw msg =
  Format.printf "Connecting to %a\n%!" Addr.pp_stream loopback;
  let sock = Net.connect ~sw loopback in
  Net.write sock msg;
  let () =
    Net_unix.with_socket_fd sock @@ fun fd -> Unix.shutdown fd SHUTDOWN_SEND
  in
  Format.printf "Response: %s\n%!" (Net.read_all sock)
```
