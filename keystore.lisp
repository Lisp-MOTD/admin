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

(deftype salt () 'string)
(deftype encrypted-key () 'string)

(adt:defdata keystore-file-types
  (keystore-public-key motd-commands:user-name motd-commands:public-key)
  (keystore-private-key salt encrypted-key))

(defun save-public-key (key user)
  "Save the public key KEY for the given USER."
  (check-type key motd-commands:dsa-public-key)
  (check-type user string)
  (let* ((filename (%user-public-key-filename user)))
    (with-open-file (*standard-output* filename
                                       :direction :output
                                       :if-does-not-exist :create)
      (prin1 (keystore-public-key user key)))))

(defun %read-public-key-file (filename)
  (let ((contents (let ((*read-eval* nil))
                    (with-open-file (*standard-input* filename)
                      (read)))))
    (assert (and (listp contents)
                 (eq (first contents) 'keystore-public-key)))
    (apply #'keystore-public-key (mapcar #'motd-commands:eval-command
                                         (rest contents)))))

(defun load-public-key (user)
  "Load the public key for the given USER."
  (check-type user string)
  (adt:with-data (keystore-public-key key-user public-key)
      (%read-public-key-file (%user-public-key-filename user))
    (assert (string= user key-user))
    public-key))

(defun save-private-key (key user password
                         &key (salt (ironclad:random-data +key-length+ *crng*))
                              (iv +iv+))
  "Save the private key KEY for the given USER.  Encrypt the stored
KEY using the given PASSWORD.  Use the given SALT to help randomize
the encrypted data.  Use the given IV as the initialization vector for
the encryption.  If the IV parameter is given, the identical IV must
be given later to load the private key."
  (check-type key motd-commands:dsa-private-key)
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
                (prin1 key)))
         (plaintext (trivial-utf-8:string-to-utf-8-bytes
                     (with-output-to-string (*standard-output*)
                       (write-string key)
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
      (prin1 (keystore-private-key (cl-base64:usb8-array-to-base64-string salt)
                                   (cl-base64:usb8-array-to-base64-string
                                    (subseq encrypted 0 n-bytes)
                                    :columns 72))))))

(defun %read-private-key-file (filename)
  (let ((contents (let ((*read-eval* nil))
                    (with-open-file (*standard-input* filename)
                      (read)))))
    (assert (and (listp contents)
                 (eq (first contents) 'keystore-private-key)))
    (apply #'keystore-private-key (mapcar #'motd-commands:eval-command
                                          (rest contents)))))

(defun load-private-key (user password
                         &key (iv +iv+))
  "Load the private key stored for the given USER.  Decrypt the stored
KEY using the given PASSWORD.  Use the given IV as the initialization
vector for the encryption.  The IV parameter must match the IV
parameter used to store the KEY."
  (check-type user string)
  (check-type password string)
  (adt:with-data (keystore-private-key salt encrypted)
      (%read-private-key-file (%user-private-key-filename user))
    (let* ((salt (cl-base64:base64-string-to-usb8-array salt))
           (encrypted (cl-base64:base64-string-to-usb8-array encrypted))

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
           (private-key (motd-commands:eval-command
                         (let ((*read-eval* nil))
                           (with-input-from-string (*standard-input*
                                                    string)
                             (read))))))
      (check-type private-key motd-commands:private-key)
      private-key)))
