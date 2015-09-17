(defpackage #:story-examples
  (:use #:cl #:clg-events #:clg-util #:findable))
(in-package #:story-examples)

;;;example define-story

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



(define-event begin standard-event
  (:after list begin)
  (:selector ()
             (if (string= (ask "Press enter!") "")
                 'space
                 '(punish begin))))

(define-event space standard-event
  (:after list begin)
  (:activator () (format t "You did it. Try the next level~%"))
  (:selector () (format t "This is the end of this test~%")))

(define-event punish standard-event
  (:after list begin)
  (:selector ()
             (format t "Nun wirst du bestraft, indem du noch öfter enter drücken musst~%")
             (read-line)
             (read-line)
             (read-line)
             nil))

(defmethod event-callable-p ((event (eql 'punish)) list)
  (with-slots (prequels) (find-instance 'event event)
    (superset-member list prequels)))

(defmethod event-disable ((event (eql 'punish)) list)
  (remove-duplicates-not list))

(defun call-story* ()
  (multiple-value-bind (active state) (call-story 'begin :reload nil)
    (loop while (print active)
       do (multiple-value-setq (active state) (call-story active :state state)))))

#|
(define-story intro
    (:selector (tell "Das Böse hat die Festungen erobert. Erobere sie zurück")
               'festungen))

(define-story festungen
    (:condition exact-prequels intro feuer/closed wasser/closed
                luft feuer wasser feuer/erobert wasser/erobert luft/erobert)
  (:selector (case (menu "Zu welcher Festung möchtest du gehen?" 'feuer 'wasser 'luft)
               (feuer '(feuer feuer/closed feuer/erobert))
               (wasser '(wasser wasser/closed wasser/erobert))
               (luft '(luft luft/erobert ende)))))

(define-story feuer/closed
    (:condition exact-prequels festungen)
  (:selector (tell "Die Feuerfestung ist geschlossen, du gehst zurück")
             'festungen))

(define-story wasser/closed
    (:condition exact-prequels festungen)
  (:selector (tell "Die Wasserfestung ist geschlossen, du gehst zurück")
             'festungen))

(define-story luft
    (:condition exact-prequels festungen)
  (:selector (tell "Du eroberst die Luftfestung, findest zwei Schlüssel und gehst zurück")
             '(festungen luft/erobert feuer wasser feuer/closed wasser/closed ende)))

(define-story feuer
    (:condition exact-prequels (luft festungen))
  (:activator (tell "Die Feuerfestung kann nun geöffnet werden"))
  (:selector (tell "Du eroberst die Feuerfestung und gehst zurück")
             '(festungen feuer/erobert ende)))

(define-story wasser
    (:condition exact-prequels (luft festungen))
  (:activator (tell "Die Wasserfestung kann nun geöffnet werden"))
  (:selector (tell "Du eroberst die Luftfestung, findest zwei Schlüssel und gehst zurück")
             '(festungen wasser/erobert ende)))

(define-story luft/erobert
    (:condition exact-prequels (luft festungen))
  (:selector (tell "Die Luftfestung ist bereits erobert, du gehst zurück")
             'festungen))

(define-story feuer/erobert
    (:condition exact-prequels (feuer festungen))
  (:selector (tell "Die Feuerfestung ist bereits erobert, du gehst zurück")
             'festungen))

(define-story wasser/erobert
    (:condition exact-prequels (wasser festungen))
  (:selector (tell "Die Wasserfestung ist bereits erobert, du gehst zurück")
             'festungen))

(define-story ende
    (:condition exact-prequels (feuer wasser luft festungen))
  (:selector (tell "Du hast den Superbösewicht bei der Luftfestung entdeckt und besiegst ihn")
             (tell "Alle sind nun glücklich. Du kannst wieder nach hause gehen")
             'luft/erobert))
|#
