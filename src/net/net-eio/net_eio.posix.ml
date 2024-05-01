type listening = Socket.listening
type streaming = Socket.streaming

module Switch = Eio.Switch

let connect ~sw stream =
  let stream = Eio_socket.Addr.stream_of_net_addr stream in
  let domain =
    Eio_unix.Net.sockaddr_to_unix stream |> Unix.domain_of_sockaddr
  in
  let socket = Eio_posix.Low_level.socket ~sw domain Unix.SOCK_STREAM 0 in
  Eio_posix.Low_level.connect socket (Eio_unix.Net.sockaddr_to_unix stream);
  Eio_socket.Streaming socket

let listen ~reuse_addr ~reuse_port ~backlog ~sw stream =
  let stream = Eio_socket.Addr.stream_of_net_addr stream in
  let sock =
    Eio_posix.Low_level.listen ~reuse_addr ~reuse_port ~backlog ~sw stream
  in
  Eio_socket.Listening sock

let accept ~sw (sock : Socket.listening Socket.t) =
  match sock with
  | Eio_socket.Listening sock ->
      let fd, sockaddr = Eio_posix.Low_level.accept ~sw sock in
      let sockaddr_unix = Eio_unix.Net.sockaddr_of_unix_stream sockaddr in
      (Eio_socket.Streaming fd, Eio_socket.Addr.stream_to_net_addr sockaddr_unix)
  | _ -> failwith "Unsupported socket type"

let fd_of_socket (type a) : a Socket.t -> Eio_unix.Fd.t = function
  | Eio_socket.Listening sock -> sock
  | Eio_socket.Streaming sock -> sock
  | _ -> failwith "Unsupported socket type"

let flow_of_socket socket = fd_of_socket socket |> Eio_posix.flow_of_fd

let write socket ba =
  let flow = flow_of_socket socket in
  Eio.Flow.copy_string ba flow

let read_all socket =
  let flow = flow_of_socket socket in
  Eio.Flow.read_all flow
