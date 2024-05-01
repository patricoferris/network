type _ Socket.t +=
  | Listening : Eio_unix.Fd.t -> Socket.listening Socket.t
  | Streaming : Eio_unix.Fd.t -> Socket.streaming Socket.t

module Addr = struct
  let convert_ip ip = Ipaddr.of_octets_exn (Obj.magic ip :> string)

  let stream_to_net_addr addr =
    match addr with
    | `Tcp (ip, port) -> `Tcp (convert_ip ip, port)
    | `Unix _ as s -> s

  let to_net_addr addr =
    match addr with
    | `Udp (ip, port) -> `Udp (convert_ip ip, port)
    | #Addr.stream as s -> stream_to_net_addr s

  let stream_of_net_addr (addr : Addr.stream) : Eio.Net.Sockaddr.stream =
    match addr with
    | `Tcp (ip, port) ->
        `Tcp (Ipaddr.to_octets ip |> Eio.Net.Ipaddr.of_raw, port)
    | `Unix _ as s -> s

  let of_net_addr (addr : [< Addr.t ]) : Eio.Net.Sockaddr.t =
    match addr with
    | `Udp (ip, port) ->
        `Udp (Ipaddr.to_string ip |> Eio.Net.Ipaddr.of_raw, port)
    | #Addr.stream as s -> (stream_of_net_addr s :> Eio.Net.Sockaddr.t)
end
