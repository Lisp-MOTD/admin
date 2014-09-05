(in-package :motd-admin)

(defmethod print-object ((sig ironclad::dsa-signature) stream)
  (if *print-readably*
      (call-next-method)
      (pprint (list :dsa-signature
                    :r (ironclad:dsa-signature-r sig)
                    :s (ironclad:dsa-signature-s sig))
              stream)))

(defun create-authenticated-message (payload)
  "Returns a list which wraps the given PAYLOAD in a way where it can
  be authenticated to come from the currently logged in USER."
  (check-type *credentials* credential)

  (let* ((plaintext (trivial-utf-8:string-to-utf-8-bytes
                     (with-output-to-string (*standard-output*)
                       (prin1 (list :time (get-universal-time)
                                    :salt (ironclad:random-bits 64 *crng*)
                                    :payload payload)))))
         (signature (ironclad:sign-message
                     (credential-private-key *credentials*)
                     (ironclad:digest-sequence :sha1 plaintext))))
    (list :user (credential-user *credentials*)
          :plaintext (cl-base64:usb8-array-to-base64-string plaintext)
          :signature signature)))
