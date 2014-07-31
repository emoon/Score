;;;; Score.asd

(asdf:defsystem #:score
  :serial t
  :depends-on (#:usocket)
  :components ((:file "package")
               (:file "main")))

