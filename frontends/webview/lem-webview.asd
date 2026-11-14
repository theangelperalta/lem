(defsystem "lem-webview"
  :depends-on ("webview"
               "float-features"
               "lem-server"
               "lem-cocoa")
  :serial t
  :components ((:file "main")
               (:file "browser")))
