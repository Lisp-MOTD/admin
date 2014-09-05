(in-package #:motd-admin)

(defvar *credentials* nil
  "Credentials of currently logged in user.")

(defclass credential ()
  ((user :initarg :user :reader credential-user
         :type motd-commands:user-name)
   (private-key :initarg :private-key :reader credential-private-key
                :type motd-commands:private-key)))

(defun login (user password &key (iv +iv+))
  (let ((private-key (load-private-key user password :iv iv)))
    (when private-key
      (setf *credentials*
            (make-instance 'credential :user user :private-key private-key)))))

(defun logout ()
  (setf *credentials* nil))
