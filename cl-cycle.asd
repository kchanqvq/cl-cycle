(asdf:defsystem cl-cycle
  :version "0.1.0"
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :maintainer "Qiantan Hong <qhong@alum.mit.edu>"
  :license "MIT"
  :description "Live music system inspired by TidalCycle"
  :serial t
  :components ((:file "packages")
               (:file "pattern")
               (:file "util")
               (:file "driver"))
  :depends-on (:alexandria
               :serapeum
               :metabang-bind
               :iterate
               :cl-collider))
