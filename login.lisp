(in-package #:motd-admin)

(defvar *credentials* nil
  "Credentials of currently logged in user.")

(defclass credential ()
  ((user :initarg :user :reader credential-user
         :type motd-commands:user-name)
   (private-key :initarg :private-key :reader credential-private-key
                :type motd-commands:private-key)))

(defun login (user password &key (iv +iv+))
  (handler-case
      (let ((private-key (load-private-key user password :iv iv)))
        (when private-key
          (setf *credentials*
                (make-instance 'credential :user user
                               :private-key private-key))))
    (file-error ()
      (warn "Unable to read private key for user ~A." user))

    (end-of-file ()
      (warn "Failed to read private key for user ~A." user))

    (trivial-utf-8:utf-8-decoding-error ()
      (warn "Error decoding private key for user ~A." user))

    (error ()
      (warn "Error reading private key for user ~A." user))))

(defun logout ()
  (setf *credentials* nil))
