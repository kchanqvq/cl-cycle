(in-package #:sc)

(defugen (dpw3-tri "DPW3Tri")
    (&optional (freq 440.0) (mul 1.0) (add 0.0))
    ((:ar (multinew new 'pure-ugen freq mul add))))

(in-package #:sc-user)

(defparameter *master-bus* nil)
(defparameter *sidechain-bus* nil)
(defparameter *drum-bus* nil)
(defparameter *echo-bus* nil)
(defparameter *samples* (make-hash-table :test 'equal))

(defun setup-server ()
  (setq *master-bus* (bus-audio :chanls 2)
        *sidechain-bus* (bus-audio :chanls 2)
        *drum-bus* (bus-audio :chanls 2)
        *echo-bus* (bus-audio :chanls 2))
  (proxy :master (let* ((sig (in.ar *master-bus* 2))
                        (sig (compander.ar sig sig 0.3 1.0 0.7 0.01 0.1 :mul 0.5))
                        (sig (clip.ar (limiter.ar sig) -1 1)))
                   (out.ar 0 sig)))
  (proxy :sidechain
         (let* ((sig (in.ar *sidechain-bus* 2))
                (sig (compander.ar sig (in.ar *drum-bus* 2)
                                   0.2 1.0 0.01 0.0 0.05)))
           (out.ar *master-bus* sig)))
  (proxy :echo
         (let* ((sig (in.ar *echo-bus* 2))
                (sig (hpf.ar sig 100))
                (sig (freeverb2.ar (car sig) (cadr sig) :mix 1.0 :room 4)))
           (out.ar *master-bus* sig)))
  (proxy :drum
         (let* ((sig (in.ar *drum-bus* 2)))
           (out.ar *master-bus* sig)))

  (defsynth saw-synth ((freq 440) (duration 4.0) (amp 0.5)
                       (cutoff 1.0) (decay 0.1) (pan 0.0)
                       (reverb 0.0) (sustain 0.2))
    (let* ((env (env-gen.kr (env (list 1.0 sustain 0)
                                 (list (* duration .2)
                                       duration))
                            :act :free))
           (fil (env-gen.kr (env (list (* 4 cutoff) cutoff) (list decay))))
           (sig (rlpf.ar (saw.ar freq env) (* freq fil) 0.5 amp))
           (sig (softclip sig))
           (sig (pan2.ar sig pan)))
      (out.ar *sidechain-bus* (* sig (- 1.0 reverb)))
      (out.ar *echo-bus* (* sig reverb))))

  (defsynth tri-synth ((freq 440) (duration 4.0) (amp 0.3)
                         (pan 0.0)
                        (reverb 0.0))
    (let* ((env (env-gen.kr (env (list 0.0 1.0 1.0 0)
                                 (list 0.02
                                       (- duration 0.02)
                                       duration))
                            :act :free))
           (sig (dpw3-tri.ar freq env))
           (sig (softclip sig))
           (sig (pan2.ar sig pan amp)))
      (out.ar *sidechain-bus* (* sig (- 1.0 reverb)))
      (out.ar *echo-bus* (* sig reverb))))

  (defsynth saw-bass ((freq 440) (duration 4.0) (preamp 3.0)
                      (amp 1.0) (cutoff 1.0) (reverb 0.0))
    (let* ((env (env-gen.kr (env (list 1.0 0.5 0)
                                 (list (min 0.2 duration)
                                       duration)
                                 (list -2 2))
                            :act :free))
           (fil (env-gen.kr (env (list cutoff (* 4 cutoff) cutoff)
                                 (list 0.01 0.5)
                                 (list 0 -5))))
           (sig (rlpf.ar (+ (pan2.ar (saw.ar (* freq 2.02)) -0.2)
                            (pan2.ar (saw.ar (* freq 2.0)) 0.0)
                            (pan2.ar (sin-osc.ar freq) 0.0)
                            (pan2.ar (saw.ar (* freq 1.98)) 0.2))
                         (* freq fil) 0.5 (* preamp env)))
           (sig (* (softclip sig) amp)))
      (out.ar *sidechain-bus* (* sig (- 1.0 reverb)))
      (out.ar *echo-bus* (* sig reverb))))
  (dolist (path (uiop:directory-files "~/Samples/Drum/"))
    (setf (gethash (pathname-name path) *samples*) (buffer-read path)))
  (defsynth drum-sampler ((bufnum 0) (duration 1.0) (amp 1.0))
    (let ((env (env-gen.kr (env (list 1.0 1.0 0)
                                (list (- duration 0.02)
                                      duration))
                           :act :free)))
      (out.ar *drum-bus* (* (play-buf.ar 2 bufnum 1.0 :act :free)
                            amp env))))
  (defsynth sampler ((bufnum 0) (duration 1.0) (amp 1.0))
    (let ((env (env-gen.kr (env (list 1.0 1.0 0)
                                (list (- duration 0.02)
                                      duration))
                           :act :free)))
      (out.ar *sidechain-bus* (* (play-buf.ar 2 bufnum 1.0 :act :free)
                            amp env)))))
