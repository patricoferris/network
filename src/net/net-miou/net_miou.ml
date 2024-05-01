module Switch = struct
  (* type t = Picos_structured.Bundle.t *)
  type t = unit

  let run ?name fn =
    let _ = ignore name in
    (* Picos_structured.Bundle.join_after fn *)
    fn (Obj.magic ())
end

let connect ~sw:_ addr =
  (* TODO: Structured concurrency with Picos *)
  let addr_unix = Miou_socket.Addr.stream_of_net_addr addr in
  let socket () =
    Unix.socket (Unix.domain_of_sockaddr addr_unix) Unix.SOCK_STREAM 0
  in
  (* let socket = Picos_structured.Finally.finally Unix.close socket in *)
  let mfd = Miou_unix.of_file_descr (socket ()) in
  Miou_unix.connect mfd addr_unix;
  Miou_socket.Streaming mfd

let listen ~reuse_addr ~reuse_port ~backlog ~sw:_ addr =
  let open Picos_structured.Finally in
  let addr_unix = Miou_socket.Addr.stream_of_net_addr addr in
  let socket () =
    Unix.socket (Unix.domain_of_sockaddr addr_unix) Unix.SOCK_STREAM 0
  in
  let@ socket = Picos_structured.Finally.finally Unix.close socket in
  Unix.setsockopt socket Unix.SO_REUSEADDR reuse_addr;
  Unix.setsockopt socket Unix.SO_REUSEPORT reuse_port;
  let mfd = Miou_unix.of_file_descr socket in
  Miou_unix.bind_and_listen ~backlog mfd addr_unix;
  Miou_socket.Listening mfd

let accept ~sw:_ = function
  | Miou_socket.Listening mfd ->
      let mfd2, sockaddr = Miou_unix.accept mfd in
      (Miou_socket.Streaming mfd2, Miou_socket.Addr.stream_to_net_addr sockaddr)
  | _ -> failwith "Invalid socket type"

let fd_of_socket (type a) : a Socket.t -> Miou_unix.file_descr = function
  | Miou_socket.Streaming mfd -> mfd
  | Miou_socket.Listening mfd -> mfd
  | _ -> failwith "Unsupported Socket Type"

let write socket msg =
  Miou_unix.write (fd_of_socket socket) msg ~off:0 ~len:(String.length msg)

let read_all socket =
  let fd = fd_of_socket socket in
  let b = Bytes.create 4096 in
  let acc = Buffer.create 128 in
  let rec loop () =
    let next_read = Miou_unix.read ~off:0 ~len:4096 fd b in
    if next_read = 0 then Buffer.contents acc
    else (
      Buffer.add_bytes acc (Bytes.sub b 0 next_read);
      loop ())
  in
  loop ()
