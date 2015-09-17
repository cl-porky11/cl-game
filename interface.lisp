
(defpackage #:interface
  (:use #:cl #:alexandria #:trivial-shell #:ltk #:ltk-mw))

(in-package #:interface)

(defvar *draw-program* "gimp")

(defun open-draw-program (&optional (filename ""))
  (shell-command (format nil "~a ~a" *draw-program* filename)))

    



;; load the cl-glut library and its dependencies (cl-opengl, cl-glu, cffi ..)
(asdf:operate 'asdf:load-op :cl-glut)


b
(defun fopen (path &aux (rel ""))
  (dolist (element (butlast (split-sequence:split-sequence #\/ path)))
    (setq rel (format nil "~a~a/" rel element))
    (shell-command (format nil "mkdir ~a" rel)))
  (open path :if-does-not-exist :create))

(defun main-menu ()
  (with-ltk ()
    (let ((frames (list (make-instance 'label :text "1")
                        (make-instance 'label :text "2")
                        (make-instance 'label :text "3"))))
      (let* ((frame (make-instance 'frame))
             (< (make-instance 'button
                               :master frame
                               :text "<"
                               :command (lambda (&aux (old (pop frames)))
                                          (pack-forget old)
                                          (pack (car frames))
                                          (appendf frames (list old)))))
             (> (make-instance 'button
                               :master frame
                               :text ">"
                               :command (lambda (&aux (new (car (last frames))))
                                          (pack-forget (car frames))
                                          (removef frames new)
                                          (pack new)
                                          (push new frames)))))
        (pack frame)
        (pack < :side :left)
        (pack > :side :left)
        (pack-forget frame)
        (pack frame)
        (pack (car frames))))))


#|
(defun window-menu (window &optional master)
  (let ((initargs (make-instance 'text)))
    (pack initargs)))
    
  
(defun main-menu (&optional master)
  (let ((window (make-instance 'button
                               :master master
                               :text "Make-Window"
                               :command (lambda ()
                                          (

|#

(defgeneric edit (object))

(defparameter *menu-list* nil)

(defclass code ()
  ((code :initarg :code)
   (function :initarg :function)))

(defmacro packs (&rest packs)
  `(progn ,@(mapcar (lambda (pack) `(pack ,@(ensure-list pack))) packs)))

(defun solid-frame (&rest instances &aux (master (make-instance 'frame)))
  (dolist (list instances)
    (destructuring-bind (instance options pack) list
      (apply 'pack (apply 'make-instance instance options) pack))))

(defmethod pack-forget ((objects list))
  (dolist (object objects)
    (pack-forget object)))

(defmethod grid-forget ((objects list))
  (dolist (object objects)
    (grid-forget object)))


(defparameter *superclass-names* (list "BALL" "MOVER" "ACTOR" "DRAWABLE"))



(defun grid-all (list)
  (let ((i 0))
    (dolist (element list)
      (grid element i 0)
      (incf i))))

(defun defclass-menu ()
  (let (class-name superclasses slots)
    (with-ltk ()
      (let (sc-entries sl-entries sl-frames-used sl-frames-unused (grid-size 0))
        (with-instances nil
            ((root frame)
             (cn frame :master root)
             (cnd label :master cn :text "Klassenname:")
             (cnt entry :master cn :text "KLASSE")
             (sc frame :master root)
             (sc-classes frame :master sc)
             (sc-add button :master sc :text "Superklasse hinzufügen"
                     :command (lambda ()
                                (with-instances nil
                                    ((widget frame
                                             :master sc-classes)
                                     (text menu-entry
                                           :content *superclass-names*
                                           :master widget)
                                     (button button
                                             :width 0
                                             :master widget
                                             :text "×"))
                                  (pack (list widget text button) :side :left)
                                  (push text sc-entries)
                                  (setf (command button)
                                        (lambda ()
                                          (removef sc-entries text)
                                          (pack-forget (list widget text button)))))))
             (sl frame :master root)
             (sl-up button :master sl
                    :text "↑"
                    :command (lambda ()
                               (grid-forget sl-frames-used)
                               (appendf sl-frames-unused (list (car sl-frames-used)))
                               (appendf sl-frames-used (list (car sl-frames-unused)))
                               (pop sl-frames-used)
                               (pop sl-frames-unused)
                               (grid-all sl-frames-used)))
             (sl-slots frame :master sl)
             (sl-down button :master sl
                    :text "↓"
                    :command (lambda ()
                               (grid-forget sl-frames-used)
                               (push (car (last sl-frames-used)) sl-frames-unused)
                               (push (car (last sl-frames-unused)) sl-frames-used)
                               (setf sl-frames-used (butlast sl-frames-used))
                               (setf sl-frames-unused (butlast sl-frames-unused))
                               (grid-all sl-frames-used)))
             (sl-add button :master sl :text "Slot hinzufügen"
                     :command (lambda ()
                                (with-instances nil
                                    ((widget frame :master sl-slots)
                                     (name entry :master widget :text "SLOT")
                                     (button button :master widget :text "×" :width 0))
                                  (pack (list name button) :side :left)
                                  (if (< grid-size 4)
                                      (progn
                                        (appendf sl-frames-used (list widget))
                                        (grid widget grid-size 0)
                                        (incf grid-size))
                                      (push widget sl-frames-unused))
                                  (push name sl-entries)
                                  (setf (command button)
                                        (lambda ()
                                          (removef sl-entries name)
                                          (removef sl-frames-used widget)
                                          (grid-forget sl-frames-used)
                                          (removef sl-frames-used widget)
                                          (if sl-frames-unused
                                              (appendf sl-frames-used (pop sl-frames-unused))
                                              (decf grid-size))
                                          (grid-all sl-frames-used)
                                          (pack-forget (list name button)))))))
             (finish button
                     :master root
                     :text "Klasse erstellen"
                     :command (secure
                               (lambda ()
                                 (setq class-name (intern (text cnt)))
                                 (setq superclasses
                                       (mapcar (lambda (text) (intern (text text)))
                                               (reverse sc-entries)))
                                 (setq slots
                                       (mapcar (lambda (name) (intern (text name)))
                                               (reverse sl-entries)))
                                 (setq *break-mainloop* t))
                               :question "Sicher?"
                               :yes "Ja" :no "Nein")))
          (pack (list root cn sc sl sl-up sl-slots sl-down sl-add finish) :fill :both)
          (pack (list cnd cnt sc-classes sc-add) :side :left :fill :both))))
    `(defclass ,class-name
         ,superclasses
       ,slots)))

(defun stop-ltk ()
  (setq *break-mainloop* t))

(defun secure (fun &key (yes "Yes") (no "No") question)
  (lambda (&aux do)
    (with-ltk ()
      (with-instances nil
          ((questionl label :text question)
           (yes button :text yes :command (lambda () (setq do t) (stop-ltk)))
           (no button :text no :command 'stop-ltk))
        (if question (pack questionl)) 
        (pack (list yes no))))
    (if do (funcall fun))))
  
                                                                

(macroexpand
 '(with-instances frame
   (cn
    (cnd label :master cn :text "Klassenname")
    (cnt entry :master cn :text "KLASSE"))))






        
