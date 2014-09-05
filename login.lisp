(in-package #:motd-admin)

(defvar *credentials* nil
  "Credentials of currently logged in user.")

(defclass credential ()
  ((user :initarg :user :reader credential-user)
   (private-key :initarg :private-key :reader credential-private-key)))

(defun login (user password &key (iv +iv+))
  (let* ((filename (%user-private-key-filename user))
         (private-key (load-private-key filename password :iv iv)))
    (when private-key
      (setf *credentials*
            (make-instance 'credential :user user :private-key private-key)))))

(defun logout ()
  (setf *credentials* nil))
