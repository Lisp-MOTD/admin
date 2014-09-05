(require :asdf)

(asdf:defsystem #:motd-admin
  :description "Lisp Message of the Day (Common Lisp Admin Tool)"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20140904"
  :license "unlicense"
  :depends-on (:drakma
               :ironclad)
  :components ((:static-file "README.md")
               (:static-file "UNLICENSE")
               (:file "package")
               (:file "keygen" :depends-on ("package"))))
