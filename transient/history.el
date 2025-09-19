((docker nil)
 (docker-compose-down
  ("--volumes"))
 (docker-compose-start nil)
 (docker-compose-stop nil)
 (docker-compose-up
  ("--build" "-d"))
 (magit-blame
  ("-w")
  ("-w" "-M")
  ("-w" "-C"))
 (magit-blame:-C)
 (magit-blame:-M)
 (magit-diff
  (("--" "src/clj/scada_ui/system.clj")
   "--no-ext-diff" "--stat"))
 (magit-dispatch nil)
 (magit-log
  ("-n256" "--graph" "--color" "--decorate")
  ("-n256" "--graph" "--color" "--decorate" "--stat")
  ("-n256"
   ("--" "src/clj/scada_ui/system.clj")
   "--graph" "--color" "--decorate" "++header")
  ("-n256"
   ("--" "src/clj/scada_ui/system.clj")
   "--graph" "--decorate"))
 (magit-shortlog
  ("--numbered" "--summary")))
