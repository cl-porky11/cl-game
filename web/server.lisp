(in-package :clg-web)

(defclass server ()
  ((host :initform "localhost")
   (port :initform *default-port* :initarg :port)
   functions
   ))

(defmethod initialize-instance :after ((server server) &key (functions '(("CHAT" . simple-chat-server))))
  (with-slots ((f functions)) server
    (setf f (alist-hash-table functions :test #'equalp))))

(defun list-public-keys (sym-key stream connections)
  (maphash (lambda (key value)
             (declare (ignore value))
             (write-until #\# (encrypt-symmetric key sym-key) stream))
           connections))

(let (encryptors)
  (defun simple-chat-server (encryptor)
    (push encryptor encryptors)
    (loop for msg = (read-encrypted-string encryptor)
       until (string= msg "")
       do (dolist (enc encryptors)
            (write-encrypted-string msg enc)
            ))
    (deletef encryptors encryptor)))

(defun run-connection (server stream)
  (with-slots (connections functions) server
    (let* (public-key
           (symmetric-key (generate-symmetric-key))
           (example (generate-symmetric-key))
           (encryptor (make-instance 'encryptor :stream stream :key symmetric-key)))
      #+nil
      (setf (gethash public-key connections) stream)
      (setq public-key (read-until #\# stream))
      (write-until #\# (encrypt-rsa symmetric-key public-key) stream)
      (finish-output stream)
      (write-encrypted-string example encryptor)
      (finish-output stream)
      (if (string= example (read-until #\# stream))
          (loop for message = (read-encrypted-string encryptor)
             do (funcall (gethash message functions)
                         encryptor))
          (error "Communication error")))))


(defun connect-server (server socket)
  (let* ((connection (socket-accept socket))
         (stream (socket-stream connection)))
    (make-thread (lambda ()
                   (run-connection server stream)
                   (socket-close connection)) :name "Connection")))

(defun run-server (server)
  (with-slots (host port) server
    (let ((socket (socket-listen host port)))
      (unwind-protect
           (loop do (connect-server server socket))
        (socket-close socket)))))

(defun start-server (&rest initargs)
  (let ((server (apply #'make-instance 'server initargs)))
    (make-thread (lambda () (run-server server)) :name "Server")))


