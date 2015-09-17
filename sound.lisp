(defpackage #:clg-sound
  (:use #:cl #:mixalot)
  (:export #:sinus-sound-function
           #:make-function-streamer
           #:start-sound
           #:stop-sound
           ))

(in-package #:clg-sound)

(main-thread-init)

(defvar *sound-mixer* (create-mixer))

(defun reset-mixer ()
  (destroy-mixer *sound-mixer*)
  (setq *sound-mixer* (create-mixer)))

(defun make-function-streamer (function)
  (lambda (streamer mixer buffer offset length time)
    (declare (ignore streamer mixer time))
    (loop for index upfrom offset repeat length
         do (stereo-incf (aref buffer index) (mono->stereo (round (* 32768 (funcall function))))))))

(defun phase-function (&rest values)
  (let* ((length (length values))
         (phases (make-array length :initial-element 0))
         (factors (map 'vector (lambda (val) (/ pi (expt 2 val))) values)))
    (lambda (&aux (last 1))
      (dotimes (i length)
        (incf (aref phases i) (* (expt last 2) (aref factors i)))
        (setq last (sin (aref phases i))))
      last)))

(defmacro sinus-sound-function (freq &optional (amp 1))
  `(let ((phase 0))
     (lambda ()
       (incf phase (/ pi ,freq))
       (* ,amp (sin phase)))))


(defun start-sound (streamer)
  (mixer-add-streamer *sound-mixer* streamer))

(defun stop-sound (streamer)
  (mixer-remove-streamer *sound-mixer* streamer))
#+nil
(defun sinus-sound-function (freq &aux (phase 0))
  (lambda (&optional new)
    (if new (setq freq new))
    (incf phase (/ pi freq))
    (sin phase)))

#+nil
(defun some-sinus-play (so freq &aux (phase 0))
  (let ((fun (sinus-sound-function freq)))
    (lambda (new)
      (incf phase (/ pi so))
      (funcall fun (+ freq (* freq 1/4 (sin phase)))))))


  
(defparameter *factor-array*
  #.(nth 0
         (list
          '(vector 24 12 16 8 12 3)
          '(vector 16 8 12 2 5)
          '(vector 23 17 11 5 2)
          '(vector 21 5 5)
          '(vector 16 8 12 4)
          '(vector 16 8 4 2)
          '(vector 17 8 12 2 5)
          '(vector 16 8 4))))

(defun custom-phase-function (&optional (factors *factor-array*))
  (let ((phases (make-array (length factors) :initial-element 0)))
    (lambda (&aux (last 1))
      (dotimes (i (length factors) last)
        (incf (aref phases i) (* (expt last 2) (/ pi (expt 2 (aref factors i)))))
        (setq last (sin (aref phases i)))))))

(defun start-phase-sounds (&rest arrays)
  (apply 'start-sound (mapcar (lambda (array) (make-function-streamer (custom-phase-function array))) arrays)))


#+nil
(let ((mixer nil))
  (defun start-sound (&optional (streamer (make-function-streamer (custom-phase-function)))
                      &rest streamers)
      (if mixer (destroy-mixer mixer))
      (setq mixer (create-mixer))
      (mixer-add-streamer mixer streamer)
      (dolist (streamer streamers)
        (mixer-add-streamer mixer streamer)))
  (defun stop-sound ()
    (if mixer (destroy-mixer mixer))))



(defun var (fun inc)
  (let ((arg 0))
    (lambda ()
      (funcall fun (incf arg inc)))))



(defun slow-sound ()
  (let ((freq 1)
        (phase 0))
    (lambda ()
      (incf phase (/ pi freq))
      (incf freq 1)
      (sin phase))))
    

