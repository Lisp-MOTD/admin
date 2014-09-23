(defpackage #:motd-admin
  (:use #:cl)
  (:export :generate-key-pair)
  (:export :login
           :logout)
  (:export :add-translation))
