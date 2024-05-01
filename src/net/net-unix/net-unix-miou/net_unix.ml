let with_socket_fd sock fn =
  let fd = Net_miou.fd_of_socket sock in
  (* TODO: Resource leaking? *)
  fn (Miou_unix.to_file_descr fd)
