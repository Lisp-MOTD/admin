(in-package :motd-admin)

(defvar *motd-url-base* "http://motd.lisp.org/")
;(defparameter *motd-url-base* "http://localhost:8000/")

(defun make-url (url-suffix)
  (concatenate 'string *motd-url-base* url-suffix))

(defun decode-returned-type (body expected-type)
  (let ((decoded (let ((*read-eval* nil))
                   (with-input-from-string (*standard-input* body)
                     (motd-commands:eval-command (ignore-errors
                                                   (read)))))))

    (if (or (motd-commands:motd-general-error-p decoded)
            (typep decoded expected-type))
        decoded
        body)))

(defun send-command (url-suffix command expected-type)
  (check-type url-suffix string)
  (check-type command motd-commands:motd-command)
  (let ((authenticated-message (create-authenticated-message command))
        (url (make-url url-suffix)))
    (multiple-value-bind (body status-code)
        (drakma:http-request url
                             :method :post
                             :parameters
                             (list (cons "command"
                                         (format nil "~S"
                                                 authenticated-message))))
      (values (decode-returned-type body expected-type)
              status-code))))
