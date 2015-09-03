(ql:quickload '(:usocket :bordeaux-threads))

(cl:defpackage #:cl-web
  (:use #:cl #:usocket #:bordeaux-threads))

;(in-package :cl-web)

(defgeneric actualise-data (game server))





(defun fixed-list-reader (stream char &optional (number (read stream t nil t))
                                                        &aux (number (1+ (or number 1))))
  (declare (ignore char))
  (let (list)
    (dotimes (i number)
      (push (read stream t nil t) list))
    (reverse list)))

(set-macro-character #\^ 'fixed-list-reader)

(set-dispatch-macro-character #\# #\@ 'fixed-list-reader)


(defparameter *cipher* (ironclad:make-cipher :aes
                                             :mode :ecb
                                             :key (string-to-octets "1234567887654321")))

(encrypt-in

(cryptos:decrypt 


(defun run-server (fun host port)
  (with-socket-listener (socket host port)
    (funcall fun socket)))

(defun run-socket (fun socket)
  (with-connected-socket (socket socket)
    (funcall fun (socket-stream socket))))

(defun run-connection (fun socket &rest args)
  (run-socket fun (apply #'socket-accept socket args)))
 
(defun run-client (fun host port &rest args)
  (run-socket fun (apply #'socket-connect host port args)))


(defun read-eval-print (stream)
  (print (eval (read stream)) stream)
  (force-output stream))

(defun read-print-line (stream &aux (line (read-line stream)))
  (princ line)
  (terpri)
  (princ (string-capitalize line) stream)
  (terpri stream)
  (force-output stream)
  (terpri))



(defun send-recieve-line (stream)
  (princ (read-line) stream)
  (terpri stream)
  (force-output stream)
  (princ (read-line stream)))

(defun send-recieve-read (stream)
  (print (read) stream)
  (force-output stream)
  (sleep 1/4)
  (print (read stream)))




(defun thread-function (&rest funs)
  (lambda (&rest args
           &aux (threads (mapcar (lambda (fun) (make-thread (lambda () (apply fun args)))) funs)))
    (dolist (thread threads)
      (join-thread thread))))

(defun client-read-line (stream)
  (loop
;     (princ "=> ")
     (write-line (read-line stream))))

(defun client-write-line (stream)
  (loop
;     (princ "<= ")
     (write-line (read-line) stream)
     (finish-output stream)))


(defparameter *nicknames* (make-hash-table :test 'equal))

(defparameter *stream-nicknames* (make-hash-table :test 'eq))

(defun server-login (stream)
  (loop
     while
        (progn
          (write-line "Choose a nickname" stream)
          (finish-output stream)
          (let* ((nick (read-line stream))
                 (password (gethash nick *nicknames*)))
            (if
             (if password
                 (progn
                   (write-line "Nickname known" stream)
                   (write-line "Type your password" stream)
                   (finish-output stream)
                   (string= (read-line stream) password))
                 (progn
                   (write-line "Nickname unknown" stream)
                   (write-line "Type a password or leave blank to choose another nickname" stream)
                   (finish-output stream)
                   (let ((password (read-line stream)))
                     (unless (string= password "")
                       (write-line "Retype your password" stream)
                       (finish-output stream)
                       (when (string= password (read-line stream))
                         (setf (gethash nick *nicknames*) password))))))
             (progn
               (setf (gethash stream *stream-nicknames*) nick)
               (write-line "Login Successful")
               nil)
             (write-line "Restarting login" stream))))))
       
(defvar *server-connections* nil)

(defun server-function (stream)
  (server-login stream)
  (push stream *server-connections*)
  (loop while (open-stream-p stream)
     do
       (let ((line (read-line stream)))
         (dolist (stream *server-connections*)
           (when (open-stream-p stream)
             (format stream "~a: ~a~%" (gethash stream *stream-nicknames* "???") line)
             (finish-output stream)))))
  (remhash stream *stream-nicknames*)
  (remove stream *server-connections*))



(defun client-function (stream)
  (funcall (thread-function 'client-read-line 'client-write-line) stream))





(defun test-client (&optional (port 2180))
  (ignore-errors
    (run-client 'client-function "127.0.0.1" port)))

(defun test-server (&optional (port 2180))
  (run-server (lambda (socket)
                (let ((socket (socket-accept socket)))
                  (make-thread (lambda () (run-socket 'server-function socket)))))
              "127.0.0.1" port))




(defun client-crypt-login (stream public-key private-key)
  (write-line public-key stream)
  (let* ((standard-key (decrypt private-key (read-line stream)))
         (test-message (decrypt standard-key (read-line stream))))
    (send test-message stream)
    standard-key))

(defun server-crypt-login (stream)
  (let ((public-key (read-line stream))
        (standard-key (generate-key))
        (message (generate-message)))
    (write-line (encrypt public-key standard-key) stream)
    (write-line (encrypt standard-key message) stream)
    (unless (equal (read-line stream) message)
      (close stream))))
    




