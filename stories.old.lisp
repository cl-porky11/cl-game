(defpackage #:clg-stories
  (:use #:cl #:clg-util #:alexandria)
  (:export #:define-story
           #:delete-story
           #:start-story
           ))
(in-package #:clg-stories)

(defvar *active-stories*)

(defstruct (story (:constructor make-story (&key activator selector)))
  prequels
  activator
  selector)

(let ((table (make-hash-table :test #'eq)))
  (defaccessor path-sequels (prequel path)
    (gethash path (or (gethash prequel table)
                      (setf (gethash prequel table)
                            (make-hash-table :test #'eq)))))
  (defun story-sequels (story)
    (hash-table-values (gethash story table)))
  (defun all-prequels ()
    (hash-table-keys table))
  (defun all-sequels ()
    (apply #'union (hash-table-values (hash-table-values table))))
  (defun delete-paths-to (story)
    (dolist (value (hash-table-values table))
      (dolist (key (hash-table-keys value))
        (deletef (gethash key value) story)))))
      
(let ((table (make-hash-table :test #'eq)))
  (defun find-story (name)
    (gethash name table))
  (defun set-story (name story)
    (check-type story story)
    (setf (gethash name table) story))
  (defun delete-story (name)
    (remhash name table)
    (delete-paths-to name)))

(defvar *path-table*)

(defaccessor story-finished-paths (story)
  (gethash story *path-table*))

(defun prequel-finished-p (prequel name)
  (dolist (preq prequel t)
    (destructuring-bind (path pname) preq
      (unless (member (list path name) (story-finished-paths pname) :test #'equal)
        (return)))))

(defun story-startable-p (name)
  (let ((story (find-story name)))
    (dolist (prequel (story-prequels story))
      (when (prequel-finished-p prequel name)
        (return t)))))

(defun test ()
  (let ((*path-table* (make-hash-table)))
    (push (list 'feuer 'feuer) (story-finished-paths 'festungen))
    ;;(print (story-finished-paths 'festungen))
    ;;(print (prequel-finished-p '((feuer festungen) (nil luft)) 'feuer))
    ;;(print (prequel-finished-p '((nil luft)) 'feuer))
    ;;(print (story-startable-p 'feuer/closed))
    (print (story-startable-p 'feuer))
    ;;(print (story-startable-p 'wasser))
    (values)))


(defun add-prequel (name path-prequel)
  (let ((story (find-story name)))
    (with-slots (prequels) story
      (pushnew path-prequel prequels :test #'set=))))

(defun ensure-story (name &key activator selector after)
  (set-story name
             (make-story :activator activator
                         :selector selector))
  (map nil
       (lambda (after-list &aux all-stories)
         (map nil
              (lambda (arg)
                (destructuring-bind (path . stories) (if (listp arg) arg (list nil arg))
                  (dolist (story stories)
                    (push name (path-sequels story path))
                    (push (list path story) all-stories))))
              (ensure-list after-list))
         (add-prequel name all-stories))
       after)
  (find-story name))

(defun activate-story (name)
  (push name *active-stories*)
  (let ((story (find-story name)))
    (dolist (prequel (story-prequels story))
      (when (prequel-finished-p prequel name)
        (dolist (preq prequel)
          (destructuring-bind (pname path) preq
            (deletef (story-finished-paths pname) (list path name) :count 1 :test #'equal)))
        (return)))
    (funcall-if (story-activator story))))

(defun call-story (name)
  (deletef *active-stories* name :test #'eq :count 1)
  (let* ((story (find-story name))
         (path (funcall-if (story-selector story))))
    (appendf (story-finished-paths name) (mapfun ((seq (path-sequels name path))) `(,path ,seq)))
    (dolist (sequel (remove-if-not #'story-startable-p (path-sequels name path)))
      (activate-story sequel))))

(flet ((expand-keyword (keyword body)
         (case keyword
           (:activator `(lambda () ,@body))
           (:selector `(lambda () ,@body))
           (:after `',body))))
  (labels ((expand-lists (lists)
             (if lists
                 (destructuring-bind ((keyword . body) . lists) lists
                   `(,keyword ,(expand-keyword keyword body) ,@(expand-lists lists))))))
    (defmacro define-story (name &rest lists)
      `(ensure-story ',name ,@(expand-lists lists)))))



(defun tell (string)
  (format t "~a~%" string)
  nil)

(defun ask (string)
  (tell string)
  (read-line))

(defun menu (string &rest answers)
  (tell string)
  (format t "~{~a~^ ~}~%" answers)
  (loop for answer = (read)
     if (member answer answers :test #'eql)
     return answer))


;;;example define-story

(define-story begin
    (:after ((wrong begin)))
  (:activator (format t "You typed to much. Try again!~%"))
  (:selector (if (string= (ask "Press enter!") "")
                 'right
                 'wrong)))

(define-story space
    (:after ((right begin)))
  (:activator (format t "You did it. Try the next level~%"))
  (:selector (format t "This is the end of this test~%")))

(define-story punish
    (:after ((wrong begin)))
  (:selector (format t "Nun wirst du bestraft, indem du noch öfter enter drücken musst~%")
             (read-line)
             (read-line)
             (read-line)))

(define-story intro
    (:selector (tell "Das Böse hat die Festungen erobert. Erobere sie zurück")))

(define-story festungen
    (:after intro feuer/closed wasser/closed luft)
  (:selector (menu "Zu welcher Festung möchtest du gehen?" 'feuer 'wasser 'luft)))

(define-story feuer/closed
    (:after ((feuer festungen)))
  (:selector (tell "Die Feuerfestung ist geschlossen, du gehst zurück")))

(define-story wasser/closed
    (:after ((wasser festungen)))
  (:selector (tell "Die Wasserfestung ist geschlossen, du gehst zurück")))

(define-story luft
    (:after ((luft festungen)))
  (:selector (tell "Du eroberst die Luftfestung, findest zwei Schlüssel und gehst zurück")))

(define-story feuer
    (:after (luft (feuer festungen)))
  (:activator (tell "Die Feuerfestung kann nun geöffnet werden"))
  (:selector (tell "Du eroberst die Feuerfestung und gehst zurück")))

(define-story wasser
    (:after (luft (wasser festungen)))
  (:activator (tell "Die Wasserfestung kann nun geöffnet werden"))
  (:selector (tell "Du eroberst die Luftfestung, findest zwei Schlüssel und gehst zurück")))

(define-story luft/erobert
    (:after (luft (luft festungen)) (luft/erobert (luft festungen)))
  (:selector (tell "Die Luftfestung ist bereits erobert, du gehst zurück")))

(define-story feuer/erobert
    (:after (luft (feuer festungen)))
  (:selector (tell "Die Feuerfestung ist bereits erobert, du gehst zurück")))

(define-story wasser/erobert
    (:after (luft (wasser festungen)))
  (:selector (tell "Die Wasserfestung ist bereits erobert, du gehst zurück")))

(define-story ende
    (:after (feuer wasser luft (luft festungen)))
  (:selector (tell "Du hast den Superbösewicht bei der Luftfestung entdeckt und besiegst ihn")
             (tell "Alle sind nun glücklich. Du kannst wieder nach hause gehen")))




(defun start-story (&key active (selector #'car)
                    &aux (*path-table* (make-hash-table :test #'eq))
                      *active-stories*)
  (dolist (active active)
    (activate-story active))
  (loop while *active-stories*
     do (call-story (funcall selector (reverse *active-stories*)))))
