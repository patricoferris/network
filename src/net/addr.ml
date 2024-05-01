type stream = [ `Tcp of Ipaddr.t * int | `Unix of string ]

let pp_stream ppf = function
  | `Tcp (addr, port) -> Format.fprintf ppf "tcp:%a:%i" Ipaddr.pp addr port
  | `Unix path -> Format.fprintf ppf "unix:%s" path

type message = [ `Udp of Ipaddr.t * int ]

let pp_message ppf = function
  | `Udp (addr, port) -> Format.fprintf ppf "tcp:%a:%i" Ipaddr.pp addr port

type t = [ stream | message ]

let pp ppf = function
  | #stream as v -> pp_stream ppf v
  | #message as v -> pp_message ppf v

module V4 = struct
  let localhost = Ipaddr.V4 Ipaddr.V4.localhost
end
