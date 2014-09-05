(require :asdf)

(asdf:defsystem #:motd-admin
  :description "Lisp Message of the Day (Common Lisp Admin Tool)"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140904"
  :license "unlicense"
  :depends-on (:drakma
               :ironclad
               :trivial-utf-8
               :cl-base64)
  :components ((:static-file "README.md")
               (:static-file "UNLICENSE")
               (:file "package")
               (:file "crng" :depends-on ("package"))
               (:file "keystore" :depends-on ("package"
                                              "crng"))
               (:file "keygen" :depends-on ("package"
                                            "crng"))
               (:file "login" :depends-on ("package"
                                           "crng"
                                           "keystore"))
               (:file "authenticate" :depends-on ("package"))))
