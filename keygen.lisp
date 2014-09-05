(in-package #:motd-admin)

(defconstant +l+ 3072)
(defconstant +n+ 256)

(defconstant +p+
  4757122097207085360194790135388628344037590339624687611188205740375433211746300182223067451208003176777099298437752863557940066182226601493689129574019805097929522688435386028990253012956286635613976197584644802760493436125549084105322284102931234006079275991498561901742690235792527742691425854270070791275421061644209814483251439474240172200029626113620051384972548286997895158383277616498607891029330513173065644551048402660554459931045975363740288260820146838784682137002402668265843527291204911402754420034748717479750107254915011815880851605008803971822745134489536312902577050849584248357010225069765331938764069054813991454142641384159333058644632377323742730825234744874632110958039152177773485730618553221403393269986426386412734530137798193380337318336585099563527287139491036654463702611521703164226331424155926636794184161820709824223600865184642318431749638680497131619631728705493444852783797432370230527776929)

(defconstant +q+
  183845405541294355033399488123604697155848585171770237387725318106434479566687)

(defconstant +g+
  4415508950929932471733690400981319962016565145590288545210424446085535898838076269798788899392281731752333952941797578054480668249931614680709726117478402550706899966357544435999355165037093047987410067273222042320328889792105954630462417590652889772630284337954739060243133797636948376639204582594874374320262712636274763329167404076546126231555004493292494077912888401910855163032205832296002306712394312181209766862117076511273280013062578446631411752907046335355466979609760371203175869596666026965082775641241066586963266243593427439208031836385060262467975663423514152948058188338456086233701808929283524823083936069324893317483922104100119402749863734336188944606096235317761535712398126528142921798958212569821506725022817144345058380910587324814117422327632312676594978814526573768938229262941159507164922368443163763421395638948240060241984243370111626463646789194928738754645889623309558524062460444278994708645436)

(defun generate-dss-pair (n l)
  (let ((2^l-1 (expt 2 (1- l))))
    (flet ((generate-q ()
             (ironclad:generate-prime n *crng*))

           (generate-p-candidate (q)
             (let* (;; Start with the first number bigger than
                    ;; 2^(bits-1) which is congruent to one modulo Q.
                    (offset (- q (mod 2^l-1 q)))
                    (base (+ 2^l-1 offset 1))

                    ;; Let the range of the random numbers be:
                    ;; floor( (2^l - base)/Q )
                    (range (truncate (- (* 2^l-1 2) base) q)))
               (+ base (* (ironclad:strong-random range *crng*) q)))))

      (loop :with q := (ironclad:generate-prime n *crng*)
         :for i :from 0
         :for p := (generate-p-candidate q)
         :until (ironclad:prime-p p *crng*)
         :when (= 50 i)
           :do (progn
                 (setf i 0
                       q (generate-q))
                 (write-char #\.)
                 (force-output))
         :finally (return (progn
                            (terpri)
                            (values p q)))))))

(defun find-g (p q)
  (loop :with power := (/ (1- p) q)
     :for h :from 2
     :for g := (ironclad:expt-mod h power p)
     :until (/= g 1)
     :finally (return g)))

(defun generate-dss-parameters ()
  (multiple-value-bind (p q) (generate-dss-pair +n+ +l+)
    (let ((g (find-g p q)))
      (values p q g))))

(defun generate-dsa-private-key ()
  (let* ((x (ironclad:strong-random +q+ *crng*))
         (y (ironclad:expt-mod +g+ x +p+)))
    (motd-commands:dsa-private-key +p+ +q+ +g+ y x)))

(defun extract-dsa-public-key (private-key)
  (check-type private-key motd-commands:dsa-private-key)
  (adt:with-data (motd-commands:dsa-private-key p q g y x) private-key
    (declare (ignore x))
    (motd-commands:dsa-public-key p q g y)))

(defun generate-key-pair (user-name password)
  "Generate a new public/private key pair for the user with the given
  USER-NAME using the given PASSWORD to encrypt the private key.  The
  private key will be stored in the file `user-name` with a `.key`
  extension.  The public key will be stored in the file `user-name`
  with a `.pub` extension."
  (let ((private-key (generate-dsa-private-key)))
    (save-private-key private-key user-name password)
    (save-public-key (extract-dsa-public-key private-key) user-name)))
