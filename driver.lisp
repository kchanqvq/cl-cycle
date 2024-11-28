(in-package #:cl-cycle)

(defvar *patterns* nil)

(defvar *offset* 0)

(defvar *cycle-per-second* 1/2)

(defun audio-loop (time)
  (cl-collider:at time
    (iter (for sym in *patterns*)
      (with cycle = (* (- time *offset*) *cycle-per-second*))
      (iter (for event in (query (get sym 'pat) cycle (1+ cycle)))
        (for event-time = (+ *offset* (/ (event-beg event) *cycle-per-second*)))
        (when (< time event-time (+ time (/ *cycle-per-second*)))
          (cl-collider:at event-time
            (apply #'cl-collider:synth
                   (append (event-value event)
                           (list :duration
                                 (/ (- (event-end event)
                                       (event-beg event))
                                    *cycle-per-second*))))))))
    (cl-collider:callback (+ time (/ *cycle-per-second*))
                          'audio-loop (+ time (/ *cycle-per-second*)))))

(defun start ()
  (setf cl-collider:*s* (cl-collider:make-external-server "localhost" :port 48800))
  (cl-collider:server-boot cl-collider:*s*)
  (cl-collider:jack-connect)
  (setf *offset* (cl-collider:now))
  (sc-user::setup-server)
  (audio-loop (cl-collider:now)))

(defun stop ()
  (cl-collider:server-quit cl-collider:*s*))
