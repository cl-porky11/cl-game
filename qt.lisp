(defun printing-object (object)
  (let ((ptr (autowrap:ptr object)))
    (tg:finalize object (lambda () (format t "(~a)" (tg:weak-pointer-value ptr))))))

(defclass closing-stream ()
  ((stream :initarg :stream)))

(defmethod initialize-instance :after (closing-stream &key)
  (tg:finalize closing-stream (let ((stream (slot-value closing-stream 'stream)))
                                (lambda () (close stream)))))

(defvar *stream* (make-instance 'closing-stream :stream (make-string-input-stream "test")))
                                
(in-package #:ltk-user)

(defun hello-1()
  (with-ltk ()
   (let ((b (make-instance 'button 
                           :master nil
                           :text "Press Me"
                           :command (lambda ()
                                      (format t "Hello World!~&")))))
     (pack b))))

(defun scribble ()
  (with-ltk ()
   (let* ((canvas (make-instance 'canvas))
          (down nil))
     (pack canvas)
     (bind canvas "<ButtonPress-1>"
           (lambda (evt)
             (setf down t)                                    
             (create-oval canvas
                      (- (event-x evt) 10) (- (event-y evt) 10)
                      (+ (event-x evt) 10) (+ (event-y evt) 10))))
     (bind canvas "<ButtonRelease-1>" (lambda (evt) 
                                        (declare (ignore evt))
                                        (setf down nil)))
     (bind canvas "<Motion>"
           (lambda (evt)
             (when down
               (create-oval canvas
                    (- (event-x evt) 10) (- (event-y evt) 10)
                    (+ (event-x evt) 10) (+ (event-y evt) 10))))))))


(defpackage #:qtools-test
  (:use #:cl+qt))

(in-package #:qtools-test)
(in-readtable :qtools)


(define-widget main-window (QWidget) ())

(define-subwidget (main-window name) (q+:make-qlineedit main-window))

(define-subwidget (main-window go) (q+:make-qpushbutton "Go!" main-window))

(define-subwidget (main-window layout) (q+:make-qhboxlayout main-window)
  (q+:add-widget layout name)
  (q+:add-widget layout go))


(define-signal (main-window name-set) (string))

(define-slot (main-window go) ()
  (declare (connected go (pressed)))
  (declare (connected name (return-pressed)))
  (signal! main-window (name-set string) (q+:text name)))

(define-slot (main-window name-set) ((new-name string))
  (declare (connected main-window (name-set string)))
  (q+:qmessagebox-information main-window "Greetings" (format NIL "Good day to you, ~a!" new-name)))



(defpackage #:qt-test
  (:use #:cl #:qt #:named-readtables))

(in-package #:qt-test)
(in-readtable :qt)

(defclass qwidget () ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((instance qwidget) &key)
  (new instance))

(defclass test-widget (qwidget)
  (edit button label)
  (:metaclass qt-class)
  (:slots ("show-name()" show-name)))

(defmethod initialize-instance :after ((instance test-widget) &key)
  (#_setWindowTitle instance "TEST")
  (setf (slot-value instance 'edit) (#_new QLineEdit "???" instance)
        (slot-value instance 'button) (#_new QPushButton "Button!" instance)
        (slot-value instance 'label) (#_new QLabel "" instance))
  (#_move (#_new QLabel "Name?" instance) 10 10)
  (#_move (slot-value instance 'edit) 10 40)
  (#_move (slot-value instance 'button) 10 70)
  (#_move (slot-value instance 'label) 10 100)
  (#_setMinimumWidth (slot-value instance 'label) 270)
  (connect (slot-value instance 'button) "clicked()" instance "show-name()"))

(defun 





