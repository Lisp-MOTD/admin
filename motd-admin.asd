(require :asdf)

(asdf:defsystem #:motd-admin
  :description "Lisp Message of the Day (Common Lisp Admin Tool)"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140904"
  :license "unlicense"
  :depends-on (:motd-commands
               :drakma
               :ironclad
               :trivial-utf-8
               :cl-base64
               :cl-algebraic-data-type)
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
               (:file "authenticate" :depends-on ("package"
                                                  "crng"
                                                  "login"))
               (:file "send" :depends-on ("package"
                                          "authenticate"))
               (:file "add-translation" :depends-on ("package"
                                                     "send"))
               (:file "delete-translation" :depends-on ("package"
                                                        "send"))
               (:file "add-tag" :depends-on ("package"
                                             "send"))
               (:file "delete-tag" :depends-on ("package"
                                                "send"))
               (:file "propose-message" :depends-on ("package"
                                                     "send"))))
