#+nil
(ql:quickload '(inferior-shell bordeaux-threads))

(defpackage #:clg-crypt
  (:use #:cl #:alexandria #:bordeaux-threads #:clg-util)
  (:export #:generate-symmetric-key
           #:generate-rsa-keypair
           
           #:encrypt-symmetric
           #:decrypt-symmetric
           #:encrypt-rsa
           #:decrypt-rsa

           #:encryptor
           #:write-encrypted-string
           #:read-encrypted-string
           ))
(in-package #:clg-crypt)

(defparameter *temp-file* "tmp")
(defparameter *temp-num* 0)

(defparameter *lock* (make-lock))

(defun string->file (string file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (princ string out)))

(defun file->string (file)
  (with-output-to-string (out)
    (with-open-file (in file :direction :input)
      (loop for char = (read-char in nil)
         while char
         do (write-char char out)))))

#+nil
(defun read-file-into-key (file)
  (with-output-to-string (out)
    (read-line)
    (loop for char = (read-char in)
       do (switch (char :test #'char=)
            (#\Newline)
            (#\- (return))
            (t (write-char char out))))))

(defun generate-symmetric-key (&optional (length 64))
  (subseq 
   (with-output-to-string (out)
     (inferior-shell:run
      (format nil "openssl rand -base64 ~a" length)
      :output out))
   0 length))

(defun generate-rsa-keypair (&optional (length 2048))
  (let* ((private
          (with-output-to-string (out)
            (inferior-shell:run
             (format nil "openssl genrsa ~a" length)
             :output out)))
         (public
          (with-output-to-string (out)
            (with-input-from-string (in private)
              (inferior-shell:run
               "openssl rsa -pubout"
               :input in
               :output out
               )))))
    (values private public)))

(defmethod encrypt-symmetric (string key &optional (type :aes256))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (inferior-shell:run
       (format nil "openssl enc -e -~a -k ~a | base64" type key)
       :input in :output out))))

(defmethod decrypt-symmetric (string key &optional (type :aes256))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (inferior-shell:run
       (format nil "base64 --decode | openssl enc -d -~a -k ~a" type key)
       :input in :output out))))


(defun encrypt-rsa (string key)
  (acquire-lock *lock*)
  (let ((tmpfile (format nil "pri~a~a" *temp-file* (incf *temp-num*))))
    (release-lock *lock*)
    (terpri)
    (string->file key tmpfile)
    (with-output-to-string (out)
      (with-input-from-string (in string)
        (inferior-shell:run
         (format nil "openssl rsautl -encrypt -inkey ~a -pubin | base64 && rm ~a" tmpfile tmpfile)
         :input in :output out)
        ))))

(defun decrypt-rsa (string key)
  (acquire-lock *lock*)
  (let ((tmp (format nil "pub~a~a" *temp-file* (incf *temp-num*))))
    (release-lock *lock*)
    (string->file key tmp)
    (with-output-to-string (out)
      (with-input-from-string (in string)
        (inferior-shell:run
         (format nil "base64 --decode | openssl rsautl -decrypt -inkey ~a && rm ~a" tmp tmp)
         :input in :output out)
        ))))

#+nil
(defun decrypt-rsa (string key)
  (string->file key *temp-file*)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (inferior-shell:run
       (format nil "base64 --decode | openssl rsautl -decrypt -inkey ~a" *temp-file*)
       :input in :output out))))

(defun test ()
  (multiple-value-bind (private public) (generate-rsa-keypair)
    (decrypt-rsa (encrypt-rsa (print (generate-symmetric-key)) public) private)))

(defun test2 ()
  (let ((key (generate-symmetric-key)))
    (decrypt-symmetric (encrypt-symmetric "data" key :aes256) key :aes256)))

(defclass encryptor ()
  ((stream :initarg :stream)
   (key :initarg :key)
   (type :initarg :type :initform :aes256)))

(defun write-encrypted-string (string encryptor)
  (with-slots (key type stream) encryptor
    (write-until #\# (encrypt-symmetric string key type) stream)
    (finish-output stream)))

(defun read-encrypted-string (encryptor)
  (with-slots (key type stream) encryptor
    (decrypt-symmetric (read-until #\# stream) key type)))





#|

(defun extended-gcd (a b)
  (if (= (mod a b) 0)
      (cons 0 1)
      (multiple-value-bind (x y) (extended-gcd b (mod a b))
        (values y (- x (* y (floor a b)))))))

(defun modulo-inverse (a n)
  (mod (car (extended-gcd a n)) n))

(defun totient (p q) (* (- p 1) (- q 1)))

(defun sqr (x) (* x x))

; modulo-power(base,exp,n) = base^exp [mod n]
(defun modulo-power (base exp n)
  (if (= exp 0)
      1
      (if (oddp exp)
          (mod (* base (modulo-power base (- exp 1) n)) n)
          (mod (sqr (modulo-power base (/ exp 2) n)) n))))




(defun exp e p q)
  (and (< 1 e) 
       (< e (totient p q))
       (= 1 (gcd e (totient p q)))))

; The private exponent is the inverse of the public exponent, mod n.
(define (private-exponent e p q) 
  (if (is-legal-public-exponent? e p q)
      (modulo-inverse e (totient p q))
      (error "Not a legal public exponent for that modulus.")))

; An encrypted message is c = m^e [mod n].
(define (encrypt m e n)
  (if (> m n)
      (error "The modulus is too small to encrypt the message.")
      (modulo-power m e n)))

; A decrypted message is m = c^d [mod n].
(define (decrypt c d n)
  (modulo-power c d n))




;; RSA example.

(define p 41)       ; A "large" prime.
(define q 47)       ; Another "large" prime.
(define n (* p q))  ; The public modulus.

(define e 7)                        ; The public exponent.
(define d (private-exponent e p q)) ; The private exponent.

(define plaintext  42)           
(define ciphertext (encrypt plaintext e n)) 

(define decrypted-ciphertext (decrypt ciphertext d n))


(display "The plaintext is:            ")
(display plaintext)
(newline)

(display "The ciphertext is:           ")
(display ciphertext)
(newline)

(display "The decrypted ciphertext is: ")
(display decrypted-ciphertext)
(newline)

(if (not (= plaintext decrypted-ciphertext))
    (error "RSA fail!"))

|#
