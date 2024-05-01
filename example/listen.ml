let () =
  Eio_main.run @@ fun _ ->
  Net.Switch.run @@ fun sw -> Net_example.listen ~sw "Goodbye from net.eio!"
