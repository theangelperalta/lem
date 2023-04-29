(defpackage :lem-vi-mode/state/ex
  (:use :cl
        :lem
        :lem-vi-mode/core)
  (:export :ex))
(in-package :lem-vi-mode/state/ex)

(defvar *ex-keymap* (make-keymap :name '*ex-keymap*))

(define-vi-state ex (:keymap *ex-keymap*))