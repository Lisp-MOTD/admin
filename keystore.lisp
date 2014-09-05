(in-package #:motd-admin)

(defconstant +key-derivation-iterations+ 8)
(defconstant +key-length+ 32)

(defconstant +iv+ #.(make-array
                   16
                   :element-type '(unsigned-byte 8)
                   :initial-contents '(60 120 232  74  18 120 178 196
                                       92 108 244 251 182 128  45  26)))

(defun save-private-key (key filename password
                         &key (salt (ironclad:random-data +key-length+ *crng*))
                              (iv +iv+))
  (let* ((cipher-key (ironclad:derive-key
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

(defun load-private-key (filename password
                         &key (iv +iv+))
  (handler-case
      (let* ((info (let ((*read-eval* nil))
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
