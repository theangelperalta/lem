(defpackage :lem-vi-mode.state.normal
  (:use :cl
        :lem
        :lem-vi-mode.core)
  (:export :normal))
(in-package :lem-vi-mode.state.normal)

(define-vi-state normal (:keymap *command-keymap*))