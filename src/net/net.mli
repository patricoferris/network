module Switch : sig
  type t

  val run : ?name:string -> (t -> 'a) -> 'a
end

val connect : sw:Switch.t -> Addr.stream -> Socket.streaming Socket.t
(** [connect ~sw t addr] is a new socket connected to remote address [addr].

    The new socket will be closed when [sw] finishes, unless closed manually first. *)

val listen :
  reuse_addr:bool ->
  reuse_port:bool ->
  backlog:int ->
  sw:Switch.t ->
  Addr.stream ->
  Socket.listening Socket.t
(** [listen ~sw ~backlog t addr] is a new listening socket bound to local address [addr].

        The new socket will be closed when [sw] finishes, unless closed manually first.

        On platforms that support this, passing port [0] will bind to a random port.

        For (non-abstract) Unix domain sockets, the path will be removed afterwards.

        @param backlog The number of pending connections that can be queued up (see listen(2)).
        @param reuse_addr Set the {!Unix.SO_REUSEADDR} socket option.
                        For Unix paths, also remove any stale left-over socket.
        @param reuse_port Set the {!Unix.SO_REUSEPORT} socket option. *)

val accept :
  sw:Switch.t ->
  Socket.listening Socket.t ->
  Socket.streaming Socket.t * Addr.stream
(** [accept ~sw socket] waits until a new connection is ready on [socket] and returns it.

        The new socket will be closed automatically when [sw] finishes, if not closed earlier.
        If you want to handle multiple connections, consider using {!accept_fork}
        instead. *)

val write : _ Socket.t -> string -> unit
val read_all : _ Socket.t -> string
