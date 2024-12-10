(in-package #:cl-cycle)

(defstruct event
  (value) (beg) (end))

(defgeneric query (pattern beg end))

(defmethod query ((self t) beg end)
  (iter (for i from (floor beg) below end)
    (collect (make-event :value self :beg i :end (1+ i)))))

(defconstant s 's)

(defmethod query ((self (eql 's)) beg end) nil)

(defstruct pattern)

(defstruct (c (:include pattern)
              (:constructor c (&rest list)))
  "Cycle through LIST of patterns."
  (list))

(defun restrict-events (events beg end)
  (mapcar (lambda (e)
            (setf (event-beg e) (max beg (event-beg e))
                  (event-end e) (min end (event-end e)))
            e)
          events))

(defmethod query ((self c) beg end)
  (iter
    (for cycle from (floor beg) below (ceiling end))
    (for (values c r) = (floor cycle (length (c-list self))))
    (nconcing
     (mapcar
      (lambda (event)
        (setf (event-beg event)
              (+ cycle (- (event-beg event) c))
              (event-end event)
              (+ cycle (- (event-end event) c)))
        event)
      (query (nth r (c-list self))
             (+ c (max (- beg cycle) 0))
             (+ (1+ c) (min 0 (- end cycle))))))))

(defstruct (j (:include pattern)
              (:constructor j (&rest list)))
  "Juxtapose LIST of patterns."
  (list))

(defmethod query ((self j) beg end)
  (mapcan (alex:rcurry #'query beg end) (j-list self)))

(defstruct (time-transform (:include pattern))
  (f) (g) (pat))

(defmethod query ((self time-transform) beg end)
  (bind ((f (time-transform-f self))
         (g (time-transform-g self))
         (beg (funcall f beg))
         (end (funcall f end)))
    (when (> beg end)
      (rotatef beg end))
    (mapcar (lambda (e)
              (setf (event-beg e) (funcall g (event-beg e))
                    (event-end e) (funcall g (event-end e)))
              (when (> (event-beg e) (event-end e))
                (rotatef (event-beg e) (event-end e)))
              e)
            (query (time-transform-pat self) beg end))))

(defun speed-transform (ratio pat)
  (make-time-transform :f (alex:curry #'* ratio)
                       :g (alex:rcurry #'/ ratio)
                       :pat pat))

(defun offset-transform (offset pat)
  (make-time-transform :f (alex:curry #'+ offset)
                       :g (alex:rcurry #'- offset)
                       :pat pat))

(defstruct (mbind (:include pattern))
  (pat) (f))

(defmethod query ((self mbind) beg end)
  (iter (for control in (query (mbind-pat self) beg end))
    (nconcing
     (restrict-events
      (query (funcall (mbind-f self) (event-value control))
             (event-beg control) (event-end control))
      (event-beg control) (event-end control)))))

(defun f (speed-pat pat)
  "Speedup PAT by SPEED-PAT."
  (make-mbind :pat speed-pat
              :f (alex:rcurry #'speed-transform pat)))

(defun off (offset-pat pat)
  "Offset PAT by OFFSET-PAT."
  (make-mbind :pat offset-pat
              :f (alex:rcurry #'offset-transform pat)))

(defun s (&rest list)
  "Play LIST of patterns in sequence."
  (speed-transform (length list) (apply #'c list)))

(defstruct (var (:include pattern))
  (symbol))

(defmethod query ((self var) beg end)
  (query (get (var-symbol self) 'pat 'm) beg end))

(defun m (function pat &rest more-pats)
  (if more-pats
      (if (pattern-p pat)
          (make-mbind :pat pat :f
                      (lambda (v) (apply #'m (alex:curry function v)
                                         more-pats)))
          (apply #'m (alex:curry function pat)
                 more-pats))
      (if (pattern-p pat)
          (make-mbind :pat pat :f function)
          (funcall function pat))))

(defun ct (&rest patterns-and-times)
  (apply #'c
         (iter (for (pattern time) on patterns-and-times by #'cddr)
           (nconcing (make-list time :initial-element pattern)))))

(defmacro defpat (var val)
  `(progn
     (define-symbol-macro ,var (make-var :symbol ',var))
     (setf (get ',var 'pat) ,val)
     ',var))
