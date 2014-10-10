(defpackage #:motd-admin
  (:use #:cl)
  (:export :generate-key-pair)
  (:export :login
           :logout)
  (:export :add-translation
           :delete-translation)
  (:export :add-tag
           :delete-tag))
;(defgeneric propose-message (message))
;(defgeneric show-proposed-messages ())
;(defgeneric publish-message (message-id))
;(defgeneric delete-message (message-id))
