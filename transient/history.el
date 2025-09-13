((docker nil)
 (docker-compose-down
  ("--volumes"))
 (docker-compose-start nil)
 (docker-compose-stop nil)
 (docker-compose-up
  ("--build" "-d")))
