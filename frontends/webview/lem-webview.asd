(defsystem "lem-webview"
  :depends-on ("webview"
               "float-features"
               "lem-server"
               (:feature (:and :darwin :arm64) "cl-objc"))
  :serial t
  :components ((:file "darwin")
               (:file "main")
               (:file "browser")))
