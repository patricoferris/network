type _ Socket.t +=
  | Listening : Miou_unix.file_descr -> Socket.listening Socket.t
  | Streaming : Miou_unix.file_descr -> Socket.streaming Socket.t

module Addr = struct
  let convert_ip ip = Ipaddr.of_octets_exn (Obj.magic ip :> string)

  let stream_to_net_addr (addr : Unix.sockaddr) =
    match addr with
    | ADDR_INET (ip, port) -> `Tcp (convert_ip ip, port)
    | ADDR_UNIX s -> `Unix s

  let unix_to_net_addr ?(tcp = true) (addr : Unix.sockaddr) =
    match addr with
    | sockaddr when tcp -> stream_to_net_addr sockaddr
    | ADDR_UNIX _ as s -> stream_to_net_addr s
    | ADDR_INET (ip, port) -> `Udp (convert_ip ip, port)

  let stream_of_net_addr = function
    | `Tcp (ip, port) ->
        Unix.ADDR_INET (Ipaddr.to_string ip |> Unix.inet_addr_of_string, port)
    | `Unix s -> Unix.ADDR_UNIX s

  let unix_of_net_addr (addr : Addr.t) =
    match addr with
    | `Udp (ip, port) ->
        Unix.ADDR_INET (Ipaddr.to_string ip |> Unix.inet_addr_of_string, port)
    | #Addr.stream as s -> stream_of_net_addr s
end
