module Fd = struct
  type t = Eio_unix.Fd.t

  let with_unix ?(message = "net.eio.unix") fn = Eio_unix.Fd.use_exn message fn
end

let fd_of_socket = Net_eio.fd_of_socket
