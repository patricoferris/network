let with_socket_fd sock fn =
  let fd = Net_eio.fd_of_socket sock in
  Eio_unix.Fd.use_exn "fd_of_socket" fd fn
