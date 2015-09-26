(in-package :clg-web)

(defclass client ()
  (private-key
   public-key
   functions
   (host :initform "localhost")
   (port :initform *default-port* :initarg :port)))

(defmethod initialize-instance :after ((client client) &key (private-key-length 2048)
                                                         (functions '(("CHAT" . simple-chat-client))))
  (with-slots (private-key public-key (f functions)) client
    (multiple-value-setq (private-key public-key) (generate-rsa-keypair private-key-length))
    (setf f (alist-hash-table functions :test #'equalp))))

(defun simple-chat-client (encryptor)
  (let ((thread
         (make-thread
          (lambda ()
            (loop
               (write-line (read-encrypted-string encryptor)))))))
    (loop for line = (read-line)
       do (write-encrypted-string line encryptor)
       if (string= line "")
       return nil)
    (destroy-thread thread)))
               
    


(defun run-client (client stream)
  (with-slots (private-key public-key functions) client
    (let (symmetric-key encryptor)
      (write-until #\# public-key stream)
      (finish-output stream)
      (setq symmetric-key (decrypt-rsa (read-until #\# stream) private-key))
      (setq encryptor (make-instance 'encryptor :key symmetric-key :stream stream))
      (write-until #\# (read-encrypted-string encryptor) stream)
      (finish-output stream)
      (loop for msg = (read-line)
         do (when-let ((fun (gethash msg functions)))
              (write-encrypted-string msg encryptor)
              (funcall fun encryptor))))))


(defun connect-client (client)
  (with-slots (host port) client
    (let ((socket (socket-connect host port)))
      (run-client client (socket-stream socket))
      (socket-close socket))))

(defun start-client (&rest initargs)
  (let ((client (apply #'make-instance 'client initargs)))
    (funcall (lambda () (connect-client client)))))



