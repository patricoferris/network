let () =
  Miou_unix.run @@ fun () ->
  Net.Switch.run @@ fun sw ->
  Net_example.connect_and_send ~sw "Hello from net.miou!"
