(** {1 Network Addresses} *)

type stream = [ `Tcp of Ipaddr.t * int | `Unix of string ]

val pp_stream : Format.formatter -> stream -> unit

type message = [ `Udp of Ipaddr.t * int ]

val pp_message : Format.formatter -> message -> unit

type t = [ stream | message ]

val pp : Format.formatter -> t -> unit

module V4 : sig
  val localhost : Ipaddr.t
end
