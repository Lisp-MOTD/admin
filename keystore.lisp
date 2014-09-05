(in-package #:motd-admin)

(defconstant +key-derivation-iterations+ 8)
(defconstant +key-length+ 32)

(defconstant +iv+ #.(make-array
                   16
                   :element-type '(unsigned-byte 8)
                   :initial-contents '(60 120 232  74  18 120 178 196
                                       92 108 244 251 182 128  45  26)))

(defun %user-public-key-filename (user)
  "Return the name of the public key file to be used for the given USER name."
  (make-pathname :directory "./"
                 :name user
                 :type "pub"))

(defun %user-private-key-filename (user)
  "Return the name of the private key file to be used for the given USER name."
  (make-pathname :directory "./"
                 :name user
                 :type "key"))

(defun save-public-key (key user)
  "Save the public key KEY for the given USER."
  (check-type key ironclad::dsa-public-key)
  (check-type user string)
  (let* ((filename (%user-public-key-filename user)))
    (with-open-file (*standard-output* filename
                                       :direction :output
                                       :if-does-not-exist :create)
      (with-standard-io-syntax
        (print (list :user user
                     :key key))))))

(defun save-private-key (key user password
                         &key (salt (ironclad:random-data +key-length+ *crng*))
                              (iv +iv+))
  "Save the private key KEY for the given USER.  Encrypt the stored
KEY using the given PASSWORD.  Use the given SALT to help randomize
the encrypted data.  Use the given IV as the initialization vector for
the encryption.  If the IV parameter is given, the identical IV must
be given later to load the private key."
  (check-type key ironclad::dsa-private-key)
  (check-type user string)
  (check-type password string)
  (let* ((filename (%user-private-key-filename user))

         (cipher-key (ironclad:derive-key
                      (ironclad:make-kdf 'ironclad:pbkdf2
                                         :digest 'ironclad:sha256)
                      (trivial-utf-8:string-to-utf-8-bytes password)
                      salt
                      +key-derivation-iterations+
                      +key-length+))
         (cipher (ironclad:make-cipher :aes
                                       :mode :cbc
                                       :key cipher-key
                                       :initialization-vector iv))
         (block-length (ironclad:block-length cipher))

         (key (with-output-to-string (*standard-output*)
                (let ((*print-pretty* t))
                  (prin1 key))))
         (plaintext (trivial-utf-8:string-to-utf-8-bytes
                     (with-output-to-string (*standard-output*)
                       (princ key)
                       (loop :repeat (- block-length
                                        (mod (length key) block-length))
                          :do (write-char #\Space)))))
         (encrypted (make-array (* block-length
                                   (ceiling (length plaintext)
                                            block-length))
                                :element-type '(unsigned-byte 8)))
         (n-bytes (nth-value 1 (ironclad:encrypt cipher plaintext encrypted))))
    (with-open-file (*standard-output* filename
                                       :direction :output
                                       :if-does-not-exist :create)
      (with-standard-io-syntax
        (print (list :salt (cl-base64:usb8-array-to-base64-string salt)
                     :encrypted (cl-base64:usb8-array-to-base64-string
                                 (subseq encrypted 0 n-bytes)
                                 :columns 72)))))))

(defun load-private-key (user password
                         &key (iv +iv+))
  "Load the private key stored for the given USER.  Decrypt the stored
KEY using the given PASSWORD.  Use the given IV as the initialization
vector for the encryption.  The IV parameter must match the IV
parameter used to store the KEY."
  (check-type user string)
  (check-type password string)
  (handler-case
      (let* ((filename (%user-private-key-filename user))
             (info (let ((*read-eval* nil))
                     (with-open-file (*standard-input* filename)
                       (read))))

             (salt (cl-base64:base64-string-to-usb8-array
                    (getf info :salt)))
             (encrypted (cl-base64:base64-string-to-usb8-array
                         (getf info :encrypted)))

             (cipher-key (ironclad:derive-key
                          (ironclad:make-kdf 'ironclad:pbkdf2
                                             :digest 'ironclad:sha256)
                          (trivial-utf-8:string-to-utf-8-bytes password)
                          salt
                          +key-derivation-iterations+
                          +key-length+))
             (cipher (ironclad:make-cipher :aes
                                           :mode :cbc
                                           :key cipher-key
                                           :initialization-vector iv))
             (plaintext (make-array (length encrypted)
                                    :element-type '(unsigned-byte 8)))

             (n-bytes (nth-value 1 (ironclad:decrypt cipher
                                                     encrypted
                                                     plaintext)))

             (string (trivial-utf-8:utf-8-bytes-to-string
                      (subseq plaintext 0 n-bytes)))
             (key-parameters (let ((*read-eval* nil))
                               (with-input-from-string (*standard-input* string)
                                 (read)))))
        (apply #'ironclad:make-private-key key-parameters))
    (error ()
      nil)))
