(in-package :motd-admin)

(defmethod print-object ((sig ironclad::dsa-signature) stream)
  (print-object (motd-commands:dsa-signature (ironclad:dsa-signature-r sig)
                                             (ironclad:dsa-signature-s sig))
                stream))

(defgeneric create-signing-key (private-key)
  (:method ((private-key motd-commands:dsa-private-key))
    (adt:with-data (motd-commands:dsa-private-key p q g y x) private-key
      (ironclad:make-private-key :dsa :p p :q q :g g :y y :x x))))

(defun create-authenticated-message (payload)
  "Returns a list which wraps the given PAYLOAD in a way where it can
  be authenticated to come from the currently logged in USER."
  (check-type payload motd-commands:motd-command)
  (check-type *credentials* credential)

  (let* ((plaintext (trivial-utf-8:string-to-utf-8-bytes
                     (with-output-to-string (*standard-output*)
                       (prin1 (list :time (get-universal-time)
                                    :salt (ironclad:random-bits 64 *crng*)
                                    :payload payload)))))
         (signature (ironclad:sign-message
                     (create-signing-key (credential-private-key *credentials*))
                     (ironclad:digest-sequence :sha1 plaintext))))
    (motd-commands:authenticated-message
     (credential-user *credentials*)
     (cl-base64:usb8-array-to-base64-string plaintext)
     (motd-commands:dsa-signature (ironclad:dsa-signature-r signature)
                                  (ironclad:dsa-signature-s signature)))))
